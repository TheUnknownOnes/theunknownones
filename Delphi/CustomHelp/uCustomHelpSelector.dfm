object FormHelpSelector: TFormHelpSelector
  Left = 0
  Top = 0
  Caption = 'Select Help Topic...'
  ClientHeight = 379
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object CategoryButtons1: TCategoryButtons
    Left = 0
    Top = 0
    Width = 617
    Height = 379
    Align = alClient
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Categories = <>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
    OnButtonClicked = CategoryButtons1ButtonClicked
    OnDrawText = CategoryButtons1DrawText
  end
end
