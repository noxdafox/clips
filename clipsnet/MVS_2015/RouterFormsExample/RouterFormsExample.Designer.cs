
namespace RouterFormsExample
{
    partial class RouterFormsExample
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
         this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
         this.autoTextBox = new global::RouterFormsExample.RouterTextBox();
         this.animalTextBox = new global::RouterFormsExample.RouterTextBox();
         this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
         this.RestartAuto = new System.Windows.Forms.Button();
         this.RestartAnimal = new System.Windows.Forms.Button();
         this.autoBackgroundWorker = new System.ComponentModel.BackgroundWorker();
         this.animalBackgroundWorker = new System.ComponentModel.BackgroundWorker();
         this.tableLayoutPanel1.SuspendLayout();
         this.flowLayoutPanel1.SuspendLayout();
         this.SuspendLayout();
         // 
         // tableLayoutPanel1
         // 
         this.tableLayoutPanel1.CellBorderStyle = System.Windows.Forms.TableLayoutPanelCellBorderStyle.Single;
         this.tableLayoutPanel1.ColumnCount = 1;
         this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
         this.tableLayoutPanel1.Controls.Add(this.autoTextBox, 0, 1);
         this.tableLayoutPanel1.Controls.Add(this.animalTextBox, 0, 2);
         this.tableLayoutPanel1.Controls.Add(this.flowLayoutPanel1, 0, 0);
         this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
         this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
         this.tableLayoutPanel1.Name = "tableLayoutPanel1";
         this.tableLayoutPanel1.RowCount = 3;
         this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 80F));
         this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
         this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
         this.tableLayoutPanel1.Size = new System.Drawing.Size(846, 647);
         this.tableLayoutPanel1.TabIndex = 0;
         // 
         // autoTextBox
         // 
         this.autoTextBox.AcceptsReturn = true;
         this.autoTextBox.AllowDrop = true;
         this.autoTextBox.BackColor = System.Drawing.SystemColors.Control;
         this.autoTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
         this.autoTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
         this.autoTextBox.Font = new System.Drawing.Font("Consolas", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         this.autoTextBox.Location = new System.Drawing.Point(4, 85);
         this.autoTextBox.Multiline = true;
         this.autoTextBox.Name = "autoTextBox";
         this.autoTextBox.ReadOnly = true;
         this.autoTextBox.Size = new System.Drawing.Size(838, 275);
         this.autoTextBox.TabIndex = 1;
         this.autoTextBox.WordWrap = false;
         // 
         // animalTextBox
         // 
         this.animalTextBox.AcceptsReturn = true;
         this.animalTextBox.AllowDrop = true;
         this.animalTextBox.BackColor = System.Drawing.SystemColors.Control;
         this.animalTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
         this.animalTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
         this.animalTextBox.Font = new System.Drawing.Font("Consolas", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         this.animalTextBox.Location = new System.Drawing.Point(4, 367);
         this.animalTextBox.Multiline = true;
         this.animalTextBox.Name = "animalTextBox";
         this.animalTextBox.ReadOnly = true;
         this.animalTextBox.Size = new System.Drawing.Size(838, 276);
         this.animalTextBox.TabIndex = 2;
         this.animalTextBox.WordWrap = false;
         // 
         // flowLayoutPanel1
         // 
         this.flowLayoutPanel1.Anchor = System.Windows.Forms.AnchorStyles.None;
         this.flowLayoutPanel1.AutoSize = true;
         this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.flowLayoutPanel1.Controls.Add(this.RestartAuto);
         this.flowLayoutPanel1.Controls.Add(this.RestartAnimal);
         this.flowLayoutPanel1.Location = new System.Drawing.Point(336, 26);
         this.flowLayoutPanel1.Name = "flowLayoutPanel1";
         this.flowLayoutPanel1.Size = new System.Drawing.Size(173, 29);
         this.flowLayoutPanel1.TabIndex = 3;
         // 
         // RestartAuto
         // 
         this.RestartAuto.AutoSize = true;
         this.RestartAuto.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.RestartAuto.Enabled = false;
         this.RestartAuto.Location = new System.Drawing.Point(3, 3);
         this.RestartAuto.Name = "RestartAuto";
         this.RestartAuto.Size = new System.Drawing.Size(76, 23);
         this.RestartAuto.TabIndex = 0;
         this.RestartAuto.Text = "Restart Auto";
         this.RestartAuto.UseVisualStyleBackColor = true;
         this.RestartAuto.Click += new System.EventHandler(this.RestartAutoClicked);
         // 
         // RestartAnimal
         // 
         this.RestartAnimal.AutoSize = true;
         this.RestartAnimal.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
         this.RestartAnimal.Enabled = false;
         this.RestartAnimal.Location = new System.Drawing.Point(85, 3);
         this.RestartAnimal.Name = "RestartAnimal";
         this.RestartAnimal.Size = new System.Drawing.Size(85, 23);
         this.RestartAnimal.TabIndex = 1;
         this.RestartAnimal.Text = "Restart Animal";
         this.RestartAnimal.UseVisualStyleBackColor = true;
         this.RestartAnimal.Click += new System.EventHandler(this.RestartAnimalClicked);
         // 
         // autoBackgroundWorker
         // 
         this.autoBackgroundWorker.WorkerSupportsCancellation = true;
         this.autoBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(this.AutoDoWork);
         this.autoBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.AutoWorkCompleted);
         // 
         // animalBackgroundWorker
         // 
         this.animalBackgroundWorker.WorkerSupportsCancellation = true;
         this.animalBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(this.AnimalDoWork);
         this.animalBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.AnimalWorkCompleted);
         // 
         // RouterForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(846, 647);
         this.Controls.Add(this.tableLayoutPanel1);
         this.Name = "RouterForm";
         this.Text = "Router Demo";
         this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.OnClosing);
         this.Load += new System.EventHandler(this.OnLoad);
         this.tableLayoutPanel1.ResumeLayout(false);
         this.tableLayoutPanel1.PerformLayout();
         this.flowLayoutPanel1.ResumeLayout(false);
         this.flowLayoutPanel1.PerformLayout();
         this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private RouterTextBox autoTextBox;
        private RouterTextBox animalTextBox;
      private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
      private System.Windows.Forms.Button RestartAuto;
      private System.Windows.Forms.Button RestartAnimal;
      private System.ComponentModel.BackgroundWorker autoBackgroundWorker;
      private System.ComponentModel.BackgroundWorker animalBackgroundWorker;
      }
}

