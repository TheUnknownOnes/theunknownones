inherited cna2ValueEditorString: Tcna2ValueEditorString
  object ed_Value: TEdit
    Left = 0
    Top = 0
    Width = 451
    Height = 21
    Align = alTop
    AutoSelect = False
    HideSelection = False
    TabOrder = 0
  end
  object mem_Hint: TMemo
    Left = 0
    Top = 21
    Width = 451
    Height = 36
    TabStop = False
    Align = alTop
    Color = clInfoBk
    Ctl3D = False
    Lines.Strings = (
      'Use a | to set the cursor position. '
      'Use 2 | to create a selection.')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
  end
end
