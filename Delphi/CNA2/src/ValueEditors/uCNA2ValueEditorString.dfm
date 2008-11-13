inherited cna2ValueEditorString: Tcna2ValueEditorString
  object Label1: TLabel
    Left = 0
    Top = 21
    Width = 451
    Height = 36
    Align = alTop
    AutoSize = False
    Caption = 
      'Use a | for setting the cursor position. Use 2 | for creating a ' +
      'selection.'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object ed_Value: TEdit
    Left = 0
    Top = 0
    Width = 451
    Height = 21
    Align = alTop
    AutoSelect = False
    TabOrder = 0
    OnEnter = ed_ValueEnter
  end
end
