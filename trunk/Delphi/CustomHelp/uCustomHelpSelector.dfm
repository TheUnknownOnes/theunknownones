object FormHelpSelector: TFormHelpSelector
  Left = 0
  Top = 0
  Caption = 'Select Help Topic for "@@HELPSTRING@@" ...'
  ClientHeight = 495
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
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object catbtnTopics: TCategoryButtons
    Left = 0
    Top = 0
    Width = 617
    Height = 372
    Align = alClient
    BorderStyle = bsNone
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Categories = <>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
    OnButtonClicked = catbtnTopicsButtonClicked
    OnDrawText = catbtnTopicsDrawText
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 372
    Width = 617
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    Color = clGradientActiveCaption
    ParentBackground = False
    TabOrder = 1
    object cbFullTextSearch: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 611
      Height = 17
      Align = alTop
      Color = clGradientActiveCaption
      ParentColor = False
      TabOrder = 0
    end
  end
  object grpErrors: TGroupBox
    Left = 0
    Top = 395
    Width = 617
    Height = 100
    Align = alBottom
    Caption = 'Errors'
    Color = clGradientActiveCaption
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    object mmoErrors: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 607
      Height = 77
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WantReturns = False
    end
  end
end
