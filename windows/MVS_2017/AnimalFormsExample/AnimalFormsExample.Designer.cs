namespace AnimalFormsExample
{
    partial class AnimalFormsExample
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
         this.autoTableLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
         this.messageLabel = new System.Windows.Forms.Label();
         this.buttonsPanel = new System.Windows.Forms.FlowLayoutPanel();
         this.prevButton = new System.Windows.Forms.Button();
         this.nextButton = new System.Windows.Forms.Button();
         this.choicesPanel = new System.Windows.Forms.FlowLayoutPanel();
         this.autoTableLayoutPanel.SuspendLayout();
         this.buttonsPanel.SuspendLayout();
         this.SuspendLayout();
         // 
         // autoTableLayoutPanel
         // 
         this.autoTableLayoutPanel.AutoSize = true;
         this.autoTableLayoutPanel.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.autoTableLayoutPanel.ColumnCount = 1;
         this.autoTableLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
         this.autoTableLayoutPanel.Controls.Add(this.messageLabel, 0, 0);
         this.autoTableLayoutPanel.Controls.Add(this.buttonsPanel, 0, 2);
         this.autoTableLayoutPanel.Controls.Add(this.choicesPanel, 0, 1);
         this.autoTableLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
         this.autoTableLayoutPanel.Location = new System.Drawing.Point(0, 0);
         this.autoTableLayoutPanel.Name = "autoTableLayoutPanel";
         this.autoTableLayoutPanel.RowCount = 3;
         this.autoTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 30F));
         this.autoTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 40F));
         this.autoTableLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 30F));
         this.autoTableLayoutPanel.Size = new System.Drawing.Size(334, 162);
         this.autoTableLayoutPanel.TabIndex = 0;
         // 
         // messageLabel
         // 
         this.messageLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)));
         this.messageLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         this.messageLabel.Location = new System.Drawing.Point(55, 0);
         this.messageLabel.Name = "messageLabel";
         this.messageLabel.Size = new System.Drawing.Size(223, 48);
         this.messageLabel.TabIndex = 0;
         this.messageLabel.Text = "label1";
         this.messageLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
         // 
         // buttonsPanel
         // 
         this.buttonsPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
         this.buttonsPanel.AutoSize = true;
         this.buttonsPanel.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.buttonsPanel.Controls.Add(this.prevButton);
         this.buttonsPanel.Controls.Add(this.nextButton);
         this.buttonsPanel.Location = new System.Drawing.Point(86, 122);
         this.buttonsPanel.Name = "buttonsPanel";
         this.buttonsPanel.Size = new System.Drawing.Size(162, 29);
         this.buttonsPanel.TabIndex = 1;
         // 
         // prevButton
         // 
         this.prevButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.prevButton.Location = new System.Drawing.Point(3, 3);
         this.prevButton.Name = "prevButton";
         this.prevButton.Size = new System.Drawing.Size(75, 23);
         this.prevButton.TabIndex = 0;
         this.prevButton.Tag = "Prev";
         this.prevButton.Text = "< Prev";
         this.prevButton.UseVisualStyleBackColor = true;
         this.prevButton.Click += new System.EventHandler(this.OnClickButton);
         // 
         // nextButton
         // 
         this.nextButton.Location = new System.Drawing.Point(84, 3);
         this.nextButton.Name = "nextButton";
         this.nextButton.Size = new System.Drawing.Size(75, 23);
         this.nextButton.TabIndex = 1;
         this.nextButton.Tag = "Next";
         this.nextButton.Text = "Next >";
         this.nextButton.UseVisualStyleBackColor = true;
         this.nextButton.Click += new System.EventHandler(this.OnClickButton);
         // 
         // choicesPanel
         // 
         this.choicesPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
         this.choicesPanel.AutoSize = true;
         this.choicesPanel.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.choicesPanel.Location = new System.Drawing.Point(167, 80);
         this.choicesPanel.Name = "choicesPanel";
         this.choicesPanel.Size = new System.Drawing.Size(0, 0);
         this.choicesPanel.TabIndex = 2;
         // 
         // AnimalForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.ClientSize = new System.Drawing.Size(334, 162);
         this.Controls.Add(this.autoTableLayoutPanel);
         this.MaximizeBox = false;
         this.MaximumSize = new System.Drawing.Size(350, 200);
         this.MinimumSize = new System.Drawing.Size(350, 200);
         this.Name = "AnimalForm";
         this.Text = "Animal Demo";
         this.autoTableLayoutPanel.ResumeLayout(false);
         this.autoTableLayoutPanel.PerformLayout();
         this.buttonsPanel.ResumeLayout(false);
         this.ResumeLayout(false);
         this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel autoTableLayoutPanel;
        private System.Windows.Forms.Label messageLabel;
        private System.Windows.Forms.FlowLayoutPanel buttonsPanel;
        private System.Windows.Forms.Button prevButton;
        private System.Windows.Forms.Button nextButton;
        private System.Windows.Forms.FlowLayoutPanel choicesPanel;
    }
}

