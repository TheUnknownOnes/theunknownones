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
    Height = 345
    Align = alClient
    BorderStyle = bsNone
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Categories = <>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    ShowHint = True
    TabOrder = 0
    OnButtonClicked = catbtnTopicsButtonClicked
    OnDrawText = catbtnTopicsDrawText
  end
  object grpErrors: TGroupBox
    Left = 0
    Top = 395
    Width = 617
    Height = 100
    Align = alBottom
    Caption = 'Errors'
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 3
    object mmoErrors: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 607
      Height = 77
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WantReturns = False
    end
  end
  object cbFullTextSearch: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 375
    Width = 611
    Height = 17
    Align = alBottom
    Color = clBtnFace
    ParentColor = False
    TabOrder = 2
  end
  object pnlSearchKeyword: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 348
    Width = 611
    Height = 21
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 1
    object lblSearchKeyword: TLabel
      Left = 0
      Top = 0
      Width = 203
      Height = 21
      Align = alLeft
      Caption = 'Keyword used in custom search providers:'
      Layout = tlCenter
    end
    object cbbSearchKeyword: TComboBox
      Left = 203
      Top = 0
      Width = 408
      Height = 21
      Align = alClient
      AutoDropDown = True
      AutoCloseUp = True
      ItemHeight = 13
      TabOrder = 0
      OnCloseUp = cbbSearchKeywordCloseUp
    end
  end
end
