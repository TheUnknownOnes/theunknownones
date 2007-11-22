

imports System.ComponentModel

Namespace System.Windows.Forms
	
	Public Class Button
		Inherits Global.System.Windows.Forms.Button
		
		Public UseVisualStyleBackColor As Boolean
		
	End Class
	
	
	
	Public Class PictureBox
		inherits Global.System.Windows.Forms.PictureBox
		
		Implements ISupportInitialize
		
		Public Sub BeginInit Implements ISupportInitialize.BeginInit 			
		End Sub
		
		Public Sub EndInit Implements ISupportInitialize.EndInit 			
		End Sub
		
		
		Public Shadows TabIndex As Int32
		
		Public Shadows TabStop As Boolean
				 		
	End Class
	
End Namespace
