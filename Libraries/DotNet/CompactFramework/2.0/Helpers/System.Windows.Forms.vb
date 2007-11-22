

Imports System.ComponentModel
Imports System.Drawing
Imports System.Windows.Forms

Namespace System.Windows.Forms
	
	Public Class Button
		Inherits Global.System.Windows.Forms.Button
	
		Public AccessibleDefaultActionDescription As String
		Public AccessibleDescription As String
		Public AccessibleName As String
		Public AllowDrop As Boolean
		Public AutoEllipsis As Boolean
		Public AutoScrollOffset As Point
		Public AutoSize As Boolean
		Public BackgroundImage As Image		
		Public CanFocus As Boolean
		Public CanSelect As Boolean
		Public CausesValidation As Boolean
		Public CheckForIllegalCrossThreadCalls As Boolean
		Public CompanyName As String
		Public Container As IContainer
		Public ContainsFocus As Boolean
		Public Created As Boolean
		Public Cursor As Cursor
		Public DefaultBackColor As Color
		Public DefaultFont As Font
		Public DefaultForeColor As Color
		Public DisplayRectangle As Rectangle
		Public Disposing As Boolean
		Public FlatAppearance As Boolean
		Public HasChildren As Boolean
		Public Image As Image
		Public ImageAlign As ContentAlignment
		Public ImageIndex As Integer
		Public ImageKey As String
		Public ImageList As ImageList
		Public IsAccessible As Boolean
		Public IsDisposed As Boolean
		Public IsHandleCreated As Boolean
		Public IsMirrored As Boolean
		Public MaximumSize As Size
		Public MinimumSize As Size
		Public PreferredSize As Size
		Public ProductName As String
		Public ProductVersion As String
		Public RecreatingHandle As Boolean
		Public Region As Region
		Public TextAlign As ContentAlignment	
		Public UseCompatibleTextRendering As Boolean
		Public UseMnemonic As Boolean
		Public UseVisualStyleBackColor As Boolean
		Public UseWaitCursor As Boolean
		
	End Class
	
	
	Public Class Label
		Inherits Global.System.Windows.Forms.Label
		
		Public AccessibleDefaultActionDescription As String
		Public AccessibleDescription As String
		Public AccessibleName As String
		Public AllowDrop As Boolean
		Public AutoEllipsis As Boolean
		Public AutoScrollOffset As Point
		Public AutoSize As Boolean
		Public BackgroundImage As Image
		Public BorderStyle As BorderStyle
		Public CanFocus As Boolean
		Public CanSelect As Boolean
		Public CausesValidation As Boolean
		Public CheckForIllegalCrossThreadCalls As Boolean
		Public CompanyName As String
		Public Container As IContainer
		Public ContainsFocus As Boolean
		Public Created As Boolean
		Public Cursor As Cursor
		Public DefaultBackColor As Color
		Public DefaultFont As Font
		Public DefaultForeColor As Color
		Public DisplayRectangle As Rectangle
		Public Disposing As Boolean
		'Public Focused As Boolean
		Public HasChildren As Boolean
		Public Image As Image
		Public ImageAlign As ContentAlignment
		Public ImageIndex As Integer
		Public ImageKey As String	
		Public IsAccessible As Boolean
		Public IsDisposed As Boolean
		Public IsHandleCreated As Boolean
		Public IsMirrored As Boolean
		Public MaximumSize As Size
		Public MinimumSize As Size
		Public PreferredHeight As Integer
		Public PreferredSize As Size
		Public PreferredWidth As Integer
		Public ProductName As String
		Public ProductVersion As String
		Public RecreatingHandle As Boolean
		Public Region As Region
		'Public TabIndex As Integer
		'Public TabStop As Boolean
		Public UseCompatibleTextRendering As Boolean
		Public UseMnemonic As Boolean
		Public UseWaitCursor As Boolean

	End Class
	
	
	Public Class PictureBox
		inherits Global.System.Windows.Forms.PictureBox
		
		Implements ISupportInitialize
		
		Public Sub BeginInit Implements ISupportInitialize.BeginInit 			
		End Sub
		
		Public Sub EndInit Implements ISupportInitialize.EndInit 			
		End Sub
		
		
		Public Shadows TabIndex As Integer
		
		Public Shadows TabStop As Boolean
				 		
	End Class
	
End Namespace
