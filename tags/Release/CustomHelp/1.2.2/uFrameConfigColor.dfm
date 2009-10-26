object FrameConfigColor: TFrameConfigColor
  Left = 0
  Top = 0
  Width = 405
  Height = 22
  AutoSize = True
  TabOrder = 0
  TabStop = True
  object catbtnTopics: TCategoryButtons
    Left = 0
    Top = 0
    Width = 254
    Height = 22
    BorderStyle = bsNone
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Enabled = False
    Categories = <
      item
        Caption = 'This is an example header'
        Color = clActiveCaption
        Collapsed = False
        Items = <>
      end>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
  end
  object ColorBox1: TColorBox
    Left = 260
    Top = 0
    Width = 145
    Height = 22
    Selected = clActiveCaption
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    ItemHeight = 16
    TabOrder = 1
    OnChange = ColorBox1Change
  end
end
