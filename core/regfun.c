   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  27/06/17            */
   /*                                                     */
   /*             Regular Expression Functions            */
   /*******************************************************/

/*************************************************************/
/* Purpose: provides regular expression functionality.       */
/*    Regular expressions are compiled and cached for better */
/*    performance.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/*************************************************************/


#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include <uthash.h>

#include "clips.h"
#include "regfun.h"


#define MATCH_OFFSET(matches, index)            \
    (matches[2 * index])
#define MATCH_LIMIT(matches, index)             \
    (matches[2 * index + 1])
#define MATCH_SIZE(matches, index)                      \
    (matches[2 * index + 1] - matches[2 * index])


#if REGEXP_CACHE_SIZE
struct regex_cache
{
    char *key;
    void *value;
    UT_hash_handle hh;
};
typedef struct regex_cache regex_cache_t;

static void *cache_get(regex_cache_t **, const char *);
static void *cache_put(regex_cache_t **, const char *, void *);
static void *cache_remove(regex_cache_t **);
static void cache_flush(regex_cache_t **, void (*)(void *));
#endif /* REGEXP_CACHE_SIZE */

static pcre2_code *regex_compile(void *, const char *, int);
static void regex_match(void *, DATA_OBJECT *, const char *, const char *, int);
static pcre2_code *regex_init(const char *, int);
static void *match_multifield(void *, const char *, pcre2_match_data *, int);
static void regex_free(pcre2_code *);
static int arguments(void *, char *, DATA_OBJECT *, DATA_OBJECT *);


/*****************************************************/
/* RegexMatch: H/L access routine                    */
/*   for the regex-match function.                   */
/*****************************************************/
void RegexMatch(void *environment, DATA_OBJECT_PTR returnValue)
{
    DATA_OBJECT pattern, string;

    if (arguments(environment, "regex-match", &pattern, &string) == 0)
        regex_match(environment, returnValue,
                    DOToString(pattern), DOToString(string), FALSE);
}


/*****************************************************/
/* RegexCaseMatch: H/L access routine                */
/*   for the regex-imatch function.                  */
/*****************************************************/
void RegexCaseMatch(void *environment, DATA_OBJECT_PTR returnValue)
{
    DATA_OBJECT pattern, string;

    if (arguments(environment, "regex-imatch", &pattern, &string) == 0)
        regex_match(environment, returnValue,
                    DOToString(pattern), DOToString(string), TRUE);
}


/****************************************************/
/* RegexCacheClear: H/L access routine              */
/*   for Clear function callback.                   */
/****************************************************/
#if REGEXP_CACHE_SIZE
void RegexCacheClear(void *environment)
{
    regex_cache_t **cache = (regex_cache_t **) &REGEX_DATA(environment)->cache;

    cache_flush(cache, (void *) regex_free);
}
#endif


/*************************************************/
/* regex_match: searches for pattern in string.  */
/*    Found results are stored in returnValue.   */
/*************************************************/
static void regex_match(void *environment, DATA_OBJECT *returnValue,
                        const char *pattern, const char *string, int caseless)
{
    void *multifield = NULL;
    pcre2_code *regex = NULL;
    pcre2_match_data *match_data = NULL;
    PCRE2_SPTR subject = (PCRE2_SPTR) string;
    int nummatch = 0, string_size = strlen(string);

    regex = regex_compile(environment, pattern, caseless);
    if (regex == NULL)
        goto error;

    match_data = pcre2_match_data_create_from_pattern(regex, NULL);
    if (match_data == NULL)
        goto error;

    nummatch = pcre2_match(regex, subject, string_size, 0, 0, match_data, NULL);
    if (nummatch <= 0)
        goto error;

    multifield = match_multifield(environment, string, match_data, nummatch);

 error:
    SetpType(returnValue, MULTIFIELD);
    SetpValue(returnValue, multifield);
    SetpDOBegin(returnValue, 1);
    SetpDOEnd(returnValue, nummatch);

#if !REGEXP_CACHE_SIZE
    regex_free(regex);
#endif

    if (match_data != NULL)
        pcre2_match_data_free(match_data);
}


/***********************************************************************/
/* regex_compile: compiles and caches the regular expression pattern.  */
/***********************************************************************/
pcre2_code *regex_compile(void *environment, const char *pattern, int caseless)
{
    int flags = caseless ? PCRE2_CASELESS : 0;

#if REGEXP_CACHE_SIZE
    int flags_size, key_size;
    pcre2_code *regex = NULL, *lru_regex = NULL;
    regex_cache_t **cache = (regex_cache_t **) &REGEX_DATA(environment)->cache;

    /* The cache key is composed as <regex-pattern><regex-flags>. */

    flags_size = floor(log10(abs(flags + 1))) + 1;
    key_size = strlen(pattern) + flags_size + 1;

    char key[key_size];

    snprintf(key, key_size, "%s%d", pattern, flags);

    /* Perform a cache lookup. If miss, compile the regex. */

    regex = (pcre2_code *) cache_get(cache, key);

    if (regex == NULL) {
        regex = regex_init(pattern, flags);

        if (regex != NULL) {
            lru_regex = (pcre2_code *) cache_put(cache, key, regex);
            regex_free(lru_regex);
        }
    }

    return regex;
#else /* Cache disabled */
    return regex_init(pattern, flags);
#endif /* REGEXP_CACHE_SIZE */
}


/***********************************************************************/
/* regex_init: compiles and optimises the regular expression pattern.  */
/***********************************************************************/
static pcre2_code *regex_init(const char *pattern, int flags)
{
    int error;
    PCRE2_SIZE offset;
    pcre2_code *regex;

    regex = pcre2_compile((PCRE2_SPTR) pattern, PCRE2_ZERO_TERMINATED, flags,
                          &error, &offset, NULL);
    if (regex == NULL)
        return NULL;

#if REGEX_JIT_COMPILE
    error = pcre2_jit_compile(regex, PCRE2_JIT_COMPLETE);
    if (error != 0) {
        pcre2_code_free(regex);
        return NULL;
    }
#endif

    return regex;
}


/****************************************************************************/
/* match_multifields: builds a Multifield data type from the regex matches. */
/****************************************************************************/
static void *match_multifield(void *environment, const char *string,
                              pcre2_match_data *match_data, int nummatch)
{
    int index;
    PCRE2_SIZE *matches = pcre2_get_ovector_pointer(match_data);
    void *multifield = EnvCreateMultifield(environment, nummatch + 1);

    for (index = 0; index < nummatch; index++) {
        int size = MATCH_SIZE(matches, index);
        int offset = MATCH_OFFSET(matches, index);
        char token[size + 1];

        memset(token, 0, (size + 1) * sizeof(char));
        strncpy(token, string + offset, size);

        SetMFType(multifield, index + 1, STRING);
        SetMFValue(multifield, index + 1, EnvAddSymbol(environment, token));
    }

    return multifield;
}


/*************************************************************/
/* regex_free: releases the regular expression's resources. */
/*************************************************************/
static void regex_free(pcre2_code *regex)
{
    if (regex != NULL)
        pcre2_code_free(regex);
}


/**************************************************************/
/* arguments: gets the arguments from the Environment.        */
/*    On success 0 is returned, -1 otherwise.                 */
/**************************************************************/
static int arguments(void *environment, char *function_name,
                     DATA_OBJECT *argument1, DATA_OBJECT *argument2)
{
    if (EnvArgCountCheck(environment, function_name, EXACTLY, 2) == -1)
        return -1;

    if (EnvArgTypeCheck(environment, function_name, 1,
                        SYMBOL_OR_STRING, argument1) == FALSE)
        return -1;

    if (EnvArgTypeCheck(environment, function_name, 2,
                        SYMBOL_OR_STRING, argument2) == FALSE)
        return -1;

    return 0;
}


#if REGEXP_CACHE_SIZE
/****************************************************/
/* cache_get: gets element from cache.              */
/*    Returns the element if found, NULL otherwise. */
/****************************************************/
static void *cache_get(regex_cache_t **cache, const char *key)
{
    regex_cache_t *entry;

    HASH_FIND_STR(*cache, key, entry);

    if (entry != NULL) {
        HASH_DELETE(hh, *cache, entry);
        HASH_ADD_KEYPTR(hh, *cache, entry->key, strlen(key), entry);

        return entry->value;
    }

    return NULL;
}


/***********************************************************************/
/* cache_put: adds the element to the cache.                           */
/*    Returns the least recently used element if full, NULL otherwise. */
/***********************************************************************/
static void *cache_put(regex_cache_t **cache, const char *key, void *value)
{
    regex_cache_t *entry = NULL;

    entry = malloc(sizeof(regex_cache_t));
    if (entry == NULL) {
        puts("Unable to allocate memory for regular expression");
        return NULL;
    }

    entry->key = malloc(strlen(key) + 1);
    if (entry->key == NULL) {
        puts("Unable to allocate memory for regular expression");
        return NULL;
    }

    strcpy(entry->key, key);
    entry->value = value;

    HASH_ADD_KEYPTR(hh, *cache, entry->key, strlen(key), entry);

    if (HASH_COUNT(*cache) >= REGEXP_CACHE_SIZE)
        return cache_remove(cache);

    return NULL;
}


/*******************************************************************/
/* cache_remove: removes the least recently used element in cache. */
/*    Returns the removed element.                                 */
/*******************************************************************/
static void *cache_remove(regex_cache_t **cache)
{
    void *lru_value = NULL;
    regex_cache_t *lru_entry, *tmp_entry;

    HASH_ITER(hh, *cache, lru_entry, tmp_entry) {
        HASH_DELETE(hh, *cache, lru_entry);

        lru_value = lru_entry->value;
        free(lru_entry->key);
        free(lru_entry);

        break;
    }

    return lru_value;
}


/***********************************************************************/
/* cache_flush: flushes the cache removing all elements.               */
/*    If not NULL, the cleanup_function can be given to free elements. */
/***********************************************************************/
static void cache_flush(regex_cache_t **cache, void (*cleanup_function)(void *))
{
    regex_cache_t *entry, *tmp_entry;

    HASH_ITER(hh, *cache, entry, tmp_entry) {
        if (cleanup_function != NULL)
            (*cleanup_function)(entry->value);

        HASH_DELETE(hh, *cache, entry);
        free(entry->key);
        free(entry);
    }
}
#endif /* REGEXP_CACHE_SIZE */
