   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  01/12/15            */
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
/* Notes: the implementation sacrifices memory for           */
/*    achieving speed.                                       */
/*                                                           */
/*************************************************************/

#include <pcre.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>

#include "regexp.h"


struct regexp
{
    pcre *re;
    pcre_extra *rextra;
};


struct regexp_cache
{
    char *key;
    void *value;
    UT_hash_handle hh;
};

typedef struct regexp_cache regexp_cache_t;


static inline regexp_t *regexp_init(const char *, int);
static inline void regexp_free(regexp_t *);
static inline void *cache_get(const char *);
static inline void *cache_put(const char *, void *);
static inline void *cache_remove();
static inline void cache_flush(void (*)(void *));


regexp_cache_t *regexp_cache = NULL;


/***********************************************************************/
/* regexp_compile: compiles and caches the regular expression pattern. */
/***********************************************************************/
regexp_t *regexp_compile(const char *pattern, int caseless)
{
    int flags = PCRE_EXTRA|PCRE_UTF8;
    int flags_size, key_size;
    regexp_t *regex = NULL, *regex_tmp = NULL;

    if (caseless)
        flags |= PCRE_CASELESS;

    /**************************************************************/
    /* The cache key is composed as <regex-pattern><regex-flags>. */
    /**************************************************************/
    flags_size = floor(log10(abs(flags))) + 1;
    key_size = strlen(pattern) + flags_size + 1;

    char key[key_size];

    snprintf(key, key_size, "%s%d", pattern, flags);

    /*********************************************************/
    /* Perform a regular expression lookup within the cache. */
    /* If not found, compile it and add it to the cache.     */
    /*********************************************************/
    regex = (regexp_t *) cache_get(key);

    if (regex == NULL) {
        regex = regexp_init(pattern, flags);

        if (regex != NULL) {
            regex_tmp = (regexp_t *) cache_put(key, regex);

            if (regex_tmp != NULL)
                regexp_free(regex_tmp);
        }
    }

    return regex;
}


/***************************************************************************/
/* regexp_matches_size: retrieves the matches size according to the regex. */
/***************************************************************************/
int regexp_matches_size(regexp_t *regex)
{
    int size;

    pcre_fullinfo(regex->re, regex->rextra, PCRE_INFO_CAPTURECOUNT, &size);

    return (size + 1) * 3;
}


/*****************************************************************************/
/* regexp_match: matches the given string with the given regular expression. */
/*    Capture groups offsets are stored within the matches array.            */
/*****************************************************************************/
int regexp_match(regexp_t *regex,
                 const char *string, int string_offset, int string_size,
                 int *matches, int matches_size)
{
    int nummatch = 0;

    if (string_offset < string_size)
        nummatch = pcre_exec(regex->re, regex->rextra, string, string_size,
                             string_offset, 0, matches, matches_size);

    return nummatch > 0 ? nummatch : 0;
}


/***********************************************************************/
/* regexp_init: compiles and optimises the regular expression pattern. */
/***********************************************************************/
static inline regexp_t *regexp_init(const char *pattern, int flags)
{
    int offset = 0;
    pcre *re = NULL;
    regexp_t *regex = NULL;
    const char *error = NULL;

    regex = (regexp_t*)malloc(sizeof(regexp_t));
    if (regex == NULL)
        return NULL;

    re = pcre_compile(pattern, flags, &error, &offset, NULL);

    if (re != NULL) {
        regex->re = re;
        regex->rextra = pcre_study(re, PCRE_STUDY_JIT_COMPILE, &error);
    } else {
        free(regex);
        regex = NULL;
    }

    return regex;
}


/****************************************************************/
/* regexp_cleanup: releases all regular expression's resources. */
/****************************************************************/
void regexp_cleanup()
{
    cache_flush((void *)regexp_free);
}


/*************************************************************/
/* regexp_free: releases the regular expression's resources. */
/*************************************************************/
static inline void regexp_free(regexp_t *regex)
{
    if (regex->re != NULL)
        pcre_free(regex->re);
    if (regex->rextra != NULL)
        pcre_free(regex->rextra);
}


/****************************************************/
/* cache_get: gets element from cache.              */
/*    Returns the element if found, NULL otherwise. */
/****************************************************/
static inline void *cache_get(const char *key)
{
    regexp_cache_t *entry;

    HASH_FIND_STR(regexp_cache, key, entry);

    if (entry != NULL) {
        HASH_DELETE(hh, regexp_cache, entry);
        HASH_ADD_KEYPTR(hh, regexp_cache, entry->key, strlen(key), entry);

        return entry->value;
    }

    return NULL;
}


/***********************************************************************/
/* cache_put: adds the element to the cache.                           */
/*    Returns the least recently used element if full, NULL otherwise. */
/***********************************************************************/
static inline void *cache_put(const char *key, void *value)
{
    regexp_cache_t *entry = NULL;

    entry = (regexp_cache_t*)malloc(sizeof(regexp_cache_t));

    if (entry != NULL) {
        entry->key = strdup(key);
        entry->value = value;

        HASH_ADD_KEYPTR(hh, regexp_cache, entry->key, strlen(key), entry);

        if (HASH_COUNT(regexp_cache) >= REGEXP_CACHE_SIZE)
            return cache_remove();
    }

    return NULL;
}


/*******************************************************************/
/* cache_remove: removes the least recently used element in cache. */
/*    Returns the removed element.                                 */
/*******************************************************************/
static inline void *cache_remove()
{
    void *lru_value = NULL;
    regexp_cache_t *lru_entry, *tmp_entry;

    HASH_ITER(hh, regexp_cache, lru_entry, tmp_entry) {
        HASH_DELETE(hh, regexp_cache, lru_entry);

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
static inline void cache_flush(void (*cleanup_function)(void *))
{
    regexp_cache_t *entry, *tmp_entry;

    HASH_ITER(hh, regexp_cache, entry, tmp_entry) {
        if (cleanup_function != NULL)
            (*cleanup_function)(entry->value);

        HASH_DELETE(hh, regexp_cache, entry);
        free(entry->key);
        free(entry);
    }
}
