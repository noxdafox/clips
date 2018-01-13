namespace WineFormsExample
{
    partial class WineFormsExample
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
         this.preferencesGroupBox = new System.Windows.Forms.GroupBox();
         this.sweetnessLabel = new System.Windows.Forms.Label();
         this.bodyLabel = new System.Windows.Forms.Label();
         this.colorLabel = new System.Windows.Forms.Label();
         this.sweetnessComboBox = new System.Windows.Forms.ComboBox();
         this.bodyComboBox = new System.Windows.Forms.ComboBox();
         this.colorComboBox = new System.Windows.Forms.ComboBox();
         this.mealGroupBox = new System.Windows.Forms.GroupBox();
         this.flavorLabel = new System.Windows.Forms.Label();
         this.sauceLabel = new System.Windows.Forms.Label();
         this.mainCourseLabel = new System.Windows.Forms.Label();
         this.flavorComboBox = new System.Windows.Forms.ComboBox();
         this.sauceComboBox = new System.Windows.Forms.ComboBox();
         this.mainCourseComboBox = new System.Windows.Forms.ComboBox();
         this.resultsDataGridView = new System.Windows.Forms.DataGridView();
         this.WineColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
         this.RecommendationWeightColumn = new Sample.DataGridViewProgressColumn();
         this.dataGridViewProgressColumn1 = new Sample.DataGridViewProgressColumn();
         this.preferencesGroupBox.SuspendLayout();
         this.mealGroupBox.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.resultsDataGridView)).BeginInit();
         this.SuspendLayout();
         // 
         // preferencesGroupBox
         // 
         this.preferencesGroupBox.Controls.Add(this.sweetnessLabel);
         this.preferencesGroupBox.Controls.Add(this.bodyLabel);
         this.preferencesGroupBox.Controls.Add(this.colorLabel);
         this.preferencesGroupBox.Controls.Add(this.sweetnessComboBox);
         this.preferencesGroupBox.Controls.Add(this.bodyComboBox);
         this.preferencesGroupBox.Controls.Add(this.colorComboBox);
         this.preferencesGroupBox.Location = new System.Drawing.Point(12, 12);
         this.preferencesGroupBox.Name = "preferencesGroupBox";
         this.preferencesGroupBox.Size = new System.Drawing.Size(210, 106);
         this.preferencesGroupBox.TabIndex = 0;
         this.preferencesGroupBox.TabStop = false;
         this.preferencesGroupBox.Text = "Preferences";
         // 
         // sweetnessLabel
         // 
         this.sweetnessLabel.AutoSize = true;
         this.sweetnessLabel.Location = new System.Drawing.Point(6, 77);
         this.sweetnessLabel.Name = "sweetnessLabel";
         this.sweetnessLabel.Size = new System.Drawing.Size(62, 13);
         this.sweetnessLabel.TabIndex = 5;
         this.sweetnessLabel.Text = "Sweetness:";
         // 
         // bodyLabel
         // 
         this.bodyLabel.AutoSize = true;
         this.bodyLabel.Location = new System.Drawing.Point(6, 50);
         this.bodyLabel.Name = "bodyLabel";
         this.bodyLabel.Size = new System.Drawing.Size(34, 13);
         this.bodyLabel.TabIndex = 4;
         this.bodyLabel.Text = "Body:";
         // 
         // colorLabel
         // 
         this.colorLabel.AutoSize = true;
         this.colorLabel.Location = new System.Drawing.Point(6, 23);
         this.colorLabel.Name = "colorLabel";
         this.colorLabel.Size = new System.Drawing.Size(34, 13);
         this.colorLabel.TabIndex = 3;
         this.colorLabel.Text = "Color:";
         // 
         // sweetnessComboBox
         // 
         this.sweetnessComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.sweetnessComboBox.FormattingEnabled = true;
         this.sweetnessComboBox.Location = new System.Drawing.Point(83, 74);
         this.sweetnessComboBox.Name = "sweetnessComboBox";
         this.sweetnessComboBox.Size = new System.Drawing.Size(121, 21);
         this.sweetnessComboBox.TabIndex = 2;
         this.sweetnessComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // bodyComboBox
         // 
         this.bodyComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.bodyComboBox.FormattingEnabled = true;
         this.bodyComboBox.Location = new System.Drawing.Point(83, 47);
         this.bodyComboBox.Name = "bodyComboBox";
         this.bodyComboBox.Size = new System.Drawing.Size(121, 21);
         this.bodyComboBox.TabIndex = 1;
         this.bodyComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // colorComboBox
         // 
         this.colorComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.colorComboBox.FormattingEnabled = true;
         this.colorComboBox.Location = new System.Drawing.Point(83, 20);
         this.colorComboBox.Name = "colorComboBox";
         this.colorComboBox.Size = new System.Drawing.Size(121, 21);
         this.colorComboBox.TabIndex = 0;
         this.colorComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // mealGroupBox
         // 
         this.mealGroupBox.Controls.Add(this.flavorLabel);
         this.mealGroupBox.Controls.Add(this.sauceLabel);
         this.mealGroupBox.Controls.Add(this.mainCourseLabel);
         this.mealGroupBox.Controls.Add(this.flavorComboBox);
         this.mealGroupBox.Controls.Add(this.sauceComboBox);
         this.mealGroupBox.Controls.Add(this.mainCourseComboBox);
         this.mealGroupBox.Location = new System.Drawing.Point(242, 12);
         this.mealGroupBox.Name = "mealGroupBox";
         this.mealGroupBox.Size = new System.Drawing.Size(210, 106);
         this.mealGroupBox.TabIndex = 1;
         this.mealGroupBox.TabStop = false;
         this.mealGroupBox.Text = "Meal";
         // 
         // flavorLabel
         // 
         this.flavorLabel.AutoSize = true;
         this.flavorLabel.Location = new System.Drawing.Point(6, 77);
         this.flavorLabel.Name = "flavorLabel";
         this.flavorLabel.Size = new System.Drawing.Size(39, 13);
         this.flavorLabel.TabIndex = 8;
         this.flavorLabel.Text = "Flavor:";
         // 
         // sauceLabel
         // 
         this.sauceLabel.AutoSize = true;
         this.sauceLabel.Location = new System.Drawing.Point(6, 50);
         this.sauceLabel.Name = "sauceLabel";
         this.sauceLabel.Size = new System.Drawing.Size(41, 13);
         this.sauceLabel.TabIndex = 7;
         this.sauceLabel.Text = "Sauce:";
         // 
         // mainCourseLabel
         // 
         this.mainCourseLabel.AutoSize = true;
         this.mainCourseLabel.Location = new System.Drawing.Point(6, 23);
         this.mainCourseLabel.Name = "mainCourseLabel";
         this.mainCourseLabel.Size = new System.Drawing.Size(69, 13);
         this.mainCourseLabel.TabIndex = 6;
         this.mainCourseLabel.Text = "Main Course:";
         // 
         // flavorComboBox
         // 
         this.flavorComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.flavorComboBox.FormattingEnabled = true;
         this.flavorComboBox.Location = new System.Drawing.Point(89, 74);
         this.flavorComboBox.Name = "flavorComboBox";
         this.flavorComboBox.Size = new System.Drawing.Size(121, 21);
         this.flavorComboBox.TabIndex = 3;
         this.flavorComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // sauceComboBox
         // 
         this.sauceComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.sauceComboBox.FormattingEnabled = true;
         this.sauceComboBox.Location = new System.Drawing.Point(89, 47);
         this.sauceComboBox.Name = "sauceComboBox";
         this.sauceComboBox.Size = new System.Drawing.Size(121, 21);
         this.sauceComboBox.TabIndex = 2;
         this.sauceComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // mainCourseComboBox
         // 
         this.mainCourseComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
         this.mainCourseComboBox.FormattingEnabled = true;
         this.mainCourseComboBox.Location = new System.Drawing.Point(89, 19);
         this.mainCourseComboBox.Name = "mainCourseComboBox";
         this.mainCourseComboBox.Size = new System.Drawing.Size(121, 21);
         this.mainCourseComboBox.TabIndex = 1;
         this.mainCourseComboBox.SelectedValueChanged += new System.EventHandler(this.OnChange);
         // 
         // resultsDataGridView
         // 
         this.resultsDataGridView.AllowUserToAddRows = false;
         this.resultsDataGridView.AllowUserToDeleteRows = false;
         this.resultsDataGridView.AllowUserToOrderColumns = true;
         this.resultsDataGridView.AllowUserToResizeRows = false;
         this.resultsDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
         this.resultsDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
         this.resultsDataGridView.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.WineColumn,
            this.RecommendationWeightColumn});
         this.resultsDataGridView.ImeMode = System.Windows.Forms.ImeMode.Disable;
         this.resultsDataGridView.Location = new System.Drawing.Point(12, 124);
         this.resultsDataGridView.Name = "resultsDataGridView";
         this.resultsDataGridView.ReadOnly = true;
         this.resultsDataGridView.RowHeadersVisible = false;
         this.resultsDataGridView.Size = new System.Drawing.Size(440, 309);
         this.resultsDataGridView.TabIndex = 2;
         // 
         // WineColumn
         // 
         this.WineColumn.DataPropertyName = "WineName";
         this.WineColumn.HeaderText = "Wine";
         this.WineColumn.Name = "WineColumn";
         this.WineColumn.ReadOnly = true;
         this.WineColumn.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
         // 
         // RecommendationWeightColumn
         // 
         this.RecommendationWeightColumn.DataPropertyName = "Certainty";
         this.RecommendationWeightColumn.HeaderText = "Recommendation Weight";
         this.RecommendationWeightColumn.Name = "RecommendationWeightColumn";
         this.RecommendationWeightColumn.ReadOnly = true;
         // 
         // dataGridViewProgressColumn1
         // 
         this.dataGridViewProgressColumn1.HeaderText = "Recommendation Weight";
         this.dataGridViewProgressColumn1.Name = "dataGridViewProgressColumn1";
         this.dataGridViewProgressColumn1.Width = 218;
         // 
         // WineForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(464, 445);
         this.Controls.Add(this.resultsDataGridView);
         this.Controls.Add(this.mealGroupBox);
         this.Controls.Add(this.preferencesGroupBox);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
         this.MaximizeBox = false;
         this.Name = "WineForm";
         this.Text = "Wine Demo";
         this.Load += new System.EventHandler(this.OnLoad);
         this.preferencesGroupBox.ResumeLayout(false);
         this.preferencesGroupBox.PerformLayout();
         this.mealGroupBox.ResumeLayout(false);
         this.mealGroupBox.PerformLayout();
         ((System.ComponentModel.ISupportInitialize)(this.resultsDataGridView)).EndInit();
         this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox preferencesGroupBox;
        private System.Windows.Forms.Label colorLabel;
        private System.Windows.Forms.ComboBox sweetnessComboBox;
        private System.Windows.Forms.ComboBox bodyComboBox;
        private System.Windows.Forms.ComboBox colorComboBox;
        private System.Windows.Forms.GroupBox mealGroupBox;
        private System.Windows.Forms.ComboBox flavorComboBox;
        private System.Windows.Forms.ComboBox sauceComboBox;
        private System.Windows.Forms.ComboBox mainCourseComboBox;
        private System.Windows.Forms.Label sweetnessLabel;
        private System.Windows.Forms.Label bodyLabel;
        private System.Windows.Forms.Label flavorLabel;
        private System.Windows.Forms.Label sauceLabel;
        private System.Windows.Forms.Label mainCourseLabel;
        private System.Windows.Forms.DataGridView resultsDataGridView;
        private Sample.DataGridViewProgressColumn dataGridViewProgressColumn1;
        private System.Windows.Forms.DataGridViewTextBoxColumn WineColumn;
        private Sample.DataGridViewProgressColumn RecommendationWeightColumn;

    }
}

