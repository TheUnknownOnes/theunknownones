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
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CategoryButtons1: TCategoryButtons
    Left = 0
    Top = 0
    Width = 617
    Height = 352
    Align = alClient
    BorderStyle = bsNone
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Categories = <>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
    OnButtonClicked = CategoryButtons1ButtonClicked
    OnDrawText = CategoryButtons1DrawText
  end
  object Panel1: TPanel
    Left = 0
    Top = 352
    Width = 617
    Height = 27
    Align = alBottom
    BevelOuter = bvNone
    Color = clGradientActiveCaption
    ParentBackground = False
    TabOrder = 1
    object cbFullTextSearch: TCheckBox
      Left = 8
      Top = 6
      Width = 577
      Height = 17
      Color = clGradientActiveCaption
      ParentColor = False
      TabOrder = 0
    end
  end
end
