object ch2FormProvider3rdPartyHelp: Tch2FormProvider3rdPartyHelp
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure 3rd Party Help Viewers for use in CustomHelp2'
  ClientHeight = 418
  ClientWidth = 717
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 41
    Width = 711
    Height = 271
    Align = alClient
    Caption = 'Currently installed Help Viewers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 39
    ExplicitHeight = 273
    object clbProviders: TCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 22
      Width = 701
      Height = 244
      OnClickCheck = clbProvidersClickCheck
      Align = alClient
      ItemHeight = 17
      TabOrder = 0
      OnClick = clbProvidersClick
      ExplicitHeight = 246
    end
  end
  object gbOptions: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 318
    Width = 711
    Height = 97
    Align = alBottom
    Caption = 'Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object LabelHeader: TLabel
      Left = 16
      Top = 41
      Width = 138
      Height = 16
      AutoSize = False
      Caption = 'Header Example'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = False
    end
    object LabelItem: TLabel
      Left = 16
      Top = 71
      Width = 138
      Height = 17
      AutoSize = False
      Caption = 'Item Example'
      Transparent = False
    end
    object Label3: TLabel
      Left = 160
      Top = 16
      Width = 64
      Height = 17
      Caption = 'Text Color'
    end
    object Label4: TLabel
      Left = 311
      Top = 16
      Width = 110
      Height = 17
      Caption = 'Background Color'
    end
    object Label5: TLabel
      Left = 470
      Top = 16
      Width = 58
      Height = 17
      Caption = 'FontStyle'
    end
    object cbHeadFC: TColorBox
      Left = 160
      Top = 40
      Width = 145
      Height = 22
      ItemHeight = 16
      TabOrder = 0
      OnChange = Headerchanged
    end
    object cbHeadBC: TColorBox
      Left = 311
      Top = 40
      Width = 145
      Height = 22
      ItemHeight = 16
      TabOrder = 1
      OnChange = Headerchanged
    end
    object cbHeadFb: TCheckBox
      Left = 470
      Top = 43
      Width = 54
      Height = 17
      Caption = 'bold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = Headerchanged
    end
    object cbHeadFi: TCheckBox
      Left = 527
      Top = 43
      Width = 54
      Height = 17
      Caption = 'italic'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 3
      OnClick = Headerchanged
    end
    object cbHeadFs: TCheckBox
      Left = 647
      Top = 43
      Width = 58
      Height = 17
      Caption = 'strike'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsStrikeOut]
      ParentFont = False
      TabOrder = 4
      OnClick = Headerchanged
    end
    object cbHeadFu: TCheckBox
      Left = 583
      Top = 43
      Width = 58
      Height = 17
      Caption = 'under'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 5
      OnClick = Headerchanged
    end
    object cbItemFC: TColorBox
      Left = 160
      Top = 68
      Width = 145
      Height = 22
      ItemHeight = 16
      TabOrder = 6
      OnChange = ItemChanged
    end
    object cbItemBC: TColorBox
      Left = 311
      Top = 68
      Width = 145
      Height = 22
      ItemHeight = 16
      TabOrder = 7
      OnChange = ItemChanged
    end
    object cbItemFb: TCheckBox
      Left = 470
      Top = 71
      Width = 53
      Height = 17
      Caption = 'bold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = ItemChanged
    end
    object cbItemFi: TCheckBox
      Left = 527
      Top = 71
      Width = 54
      Height = 17
      Caption = 'italic'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 9
      OnClick = ItemChanged
    end
    object cbItemFu: TCheckBox
      Left = 583
      Top = 71
      Width = 58
      Height = 17
      Caption = 'under'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 10
      OnClick = ItemChanged
    end
    object cbItemFs: TCheckBox
      Left = 647
      Top = 71
      Width = 58
      Height = 17
      Caption = 'strike'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = [fsStrikeOut]
      ParentFont = False
      TabOrder = 11
      OnClick = ItemChanged
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 711
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 10
      Width = 43
      Height = 17
      Caption = 'Priority'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EditPrio: TSpinEdit
      Left = 64
      Top = 4
      Width = 121
      Height = 27
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 0
      MinValue = 0
      ParentFont = False
      TabOrder = 0
      Value = 0
      OnChange = EditPrioChange
    end
  end
end
