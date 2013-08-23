object Form46: TForm46
  Left = 0
  Top = 0
  Caption = 'ZintTest'
  ClientHeight = 509
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgResult: TImage
    AlignWithMargins = True
    Left = 3
    Top = 132
    Width = 771
    Height = 355
    Align = alClient
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = False
    Stretch = True
    ExplicitLeft = -2
    ExplicitTop = 44
    ExplicitWidth = 696
    ExplicitHeight = 300
  end
  object lblError: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 493
    Width = 771
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edData: TEdit
      Left = 8
      Top = 8
      Width = 296
      Height = 21
      TabOrder = 0
      Text = '123456'
      OnChange = edDataChange
    end
    object comType: TComboBox
      Left = 310
      Top = 8
      Width = 146
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = comTypeChange
    end
    object btPrint: TButton
      Left = 613
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Print'
      TabOrder = 2
      OnClick = btPrintClick
    end
    object comPrinter: TComboBox
      Left = 462
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object btSVG: TButton
      Left = 694
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Save SVG'
      TabOrder = 4
      OnClick = btSVGClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 35
    Width = 777
    Height = 94
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Left = 207
      Top = 32
      Width = 97
      Height = 13
      Caption = 'Render adjust Mode'
    end
    object Label4: TLabel
      Left = 207
      Top = 59
      Width = 152
      Height = 13
      Caption = 'Maxicode Hexagon Scale (float)'
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = -1
      Width = 185
      Height = 92
      Caption = 'Frames'
      TabOrder = 0
      object Label2: TLabel
        Left = 15
        Top = 68
        Width = 56
        Height = 13
        Caption = 'Whitespace'
      end
      object Label1: TLabel
        Left = 15
        Top = 41
        Width = 59
        Height = 13
        Caption = 'Frame width'
      end
      object edWhitespaceWidth: TEdit
        Left = 80
        Top = 65
        Width = 94
        Height = 21
        TabOrder = 0
        Text = '1'
        OnChange = edDataChange
      end
      object edFrameWidth: TEdit
        Left = 80
        Top = 38
        Width = 94
        Height = 21
        TabOrder = 1
        Text = '1'
        OnChange = edDataChange
      end
      object rbBox: TRadioButton
        Left = 15
        Top = 18
        Width = 42
        Height = 17
        Caption = 'Box'
        TabOrder = 2
        OnClick = edDataChange
      end
      object rbBind: TRadioButton
        Left = 74
        Top = 18
        Width = 42
        Height = 17
        Caption = 'Bind'
        TabOrder = 3
        OnClick = edDataChange
      end
      object rbNone: TRadioButton
        Left = 128
        Top = 18
        Width = 42
        Height = 17
        Caption = 'None'
        Checked = True
        TabOrder = 4
        TabStop = True
        OnClick = edDataChange
      end
    end
    object cbHRT: TCheckBox
      Left = 207
      Top = 6
      Width = 162
      Height = 17
      Caption = 'show human readable text'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = edDataChange
    end
    object cbRAM: TComboBox
      Left = 310
      Top = 29
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 2
      Text = 'Scale'
      OnChange = edDataChange
      Items.Strings = (
        'None'
        'Scale'
        'Inflate Image')
    end
    object edMHS: TEdit
      Left = 365
      Top = 56
      Width = 37
      Height = 21
      TabOrder = 3
      Text = '1'
      OnChange = edDataChange
    end
    object ButtonFont: TButton
      Left = 480
      Top = 6
      Width = 208
      Height = 44
      Caption = 'change Text Font'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = ButtonFontClick
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 384
    Top = 256
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 456
    Top = 256
  end
end
