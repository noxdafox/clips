﻿using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;

// Source: https://social.msdn.microsoft.com/Forums/windows/en-US/769ca9d6-1e9d-4d76-8c23-db535b2f19c2/sample-code-datagridview-progress-bar-column?forum=winformsdatacontrols

namespace Sample {
    public class DataGridViewProgressColumn : DataGridViewImageColumn {
        public DataGridViewProgressColumn() {
            CellTemplate = new DataGridViewProgressCell();
        }
    }
}
namespace Sample {
    class DataGridViewProgressCell : DataGridViewImageCell {
        // Used to make custom cell consistent with a DataGridViewImageCell
        static Image emptyImage;
        static DataGridViewProgressCell() {
            emptyImage = new Bitmap(1, 1, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
        }
        public DataGridViewProgressCell() {
            this.ValueType = typeof(int);
        }
        // Method required to make the Progress Cell consistent with the default Image Cell. 
        // The default Image Cell assumes an Image as a value, although the value of the Progress Cell is an int.
        protected override object GetFormattedValue(object value,
                            int rowIndex, ref DataGridViewCellStyle cellStyle,
                            TypeConverter valueTypeConverter,
                            TypeConverter formattedValueTypeConverter,
                            DataGridViewDataErrorContexts context) {
            return emptyImage;
        }

  
        protected override void Paint(System.Drawing.Graphics g, System.Drawing.Rectangle clipBounds, System.Drawing.Rectangle cellBounds, int rowIndex, 
            DataGridViewElementStates cellState, object value, object formattedValue, string errorText, DataGridViewCellStyle cellStyle, 
            DataGridViewAdvancedBorderStyle advancedBorderStyle, DataGridViewPaintParts paintParts) {
            if (null == value) value = 0;
             int progressVal;
            if (value != null)
                progressVal = (int) value;
            else
                progressVal = 1; 

            float percentage = ((float)progressVal / 100.0f); // Need to convert to float before division; otherwise C# returns int which is 0 for anything but 100%.
            Brush backColorBrush = new SolidBrush(cellStyle.BackColor);
            Brush foreColorBrush = new SolidBrush(cellStyle.ForeColor);
            // Draws the cell grid
            base.Paint(g, clipBounds, cellBounds,
             rowIndex, cellState, value, formattedValue, errorText,
             cellStyle, advancedBorderStyle, (paintParts & ~DataGridViewPaintParts.ContentForeground));
            if (percentage > 0.0) {
                // Draw the progress bar and the text
                g.FillRectangle(new SolidBrush(Color.FromArgb(163, 189, 242)), cellBounds.X + 2, cellBounds.Y + 2, Convert.ToInt32((percentage * cellBounds.Width - 4)), cellBounds.Height - 4);
                g.DrawString(progressVal.ToString() + "%", cellStyle.Font, foreColorBrush, cellBounds.X + 6, cellBounds.Y + 4); 
            } else {
                // draw the text
                if (null != this.DataGridView.CurrentRow && this.DataGridView.CurrentRow.Index == rowIndex)
                    g.DrawString(progressVal.ToString() + "%", cellStyle.Font, new SolidBrush(cellStyle.SelectionForeColor), cellBounds.X + 6, cellBounds.Y + 4);
                else
                    g.DrawString(progressVal.ToString() + "%", cellStyle.Font, foreColorBrush, cellBounds.X + 6, cellBounds.Y + 4);
            }
        }
    }
}
