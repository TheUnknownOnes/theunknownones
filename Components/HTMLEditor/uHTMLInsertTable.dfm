object form_ITInsertTable: Tform_ITInsertTable
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Neue Tabelle'
  ClientHeight = 356
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pan_Bottom: TPanel
    Left = 0
    Top = 325
    Width = 268
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      268
      31)
    object btn_OK: TBitBtn
      Left = 101
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TBitBtn
      Left = 182
      Top = 3
      Width = 83
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object gb_Dimension: TGroupBox
    Left = 0
    Top = 35
    Width = 268
    Height = 89
    Align = alTop
    Caption = 'Dimension'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 23
      Width = 32
      Height = 13
      Caption = 'Zeilen:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 55
      Width = 40
      Height = 13
      Caption = 'Spalten:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ed_Rows: TJvValidateEdit
      Left = 64
      Top = 20
      Width = 81
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object ed_Cols: TJvValidateEdit
      Left = 64
      Top = 52
      Width = 81
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object gb_PropTable: TGroupBox
    Left = 0
    Top = 124
    Width = 268
    Height = 191
    Align = alTop
    Caption = 'Tabelleneigenschaften'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 72
      Height = 13
      Caption = 'Tabellenbreite:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 168
      Top = 24
      Width = 11
      Height = 13
      Caption = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 115
      Width = 89
      Height = 26
      Caption = 'Abstand zwischen Zelle und Rand:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label6: TLabel
      Left = 168
      Top = 122
      Width = 12
      Height = 13
      Caption = 'px'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 155
      Width = 89
      Height = 26
      Caption = 'Abstand zwischen Zelle und Zelle:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label8: TLabel
      Left = 168
      Top = 164
      Width = 12
      Height = 13
      Caption = 'px'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 8
      Top = 56
      Width = 71
      Height = 13
      Caption = 'Rahmenbreite:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 8
      Top = 88
      Width = 69
      Height = 13
      Caption = 'Rahmenfarbe:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ed_TableWidth: TJvSpinEdit
      Left = 101
      Top = 21
      Width = 65
      Height = 21
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 100.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object ed_CellPadding: TJvSpinEdit
      Left = 101
      Top = 117
      Width = 65
      Height = 21
      MaxValue = 100.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object ed_CellSpacing: TJvSpinEdit
      Left = 101
      Top = 159
      Width = 65
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object ed_BorderWidth: TJvSpinEdit
      Left = 101
      Top = 53
      Width = 65
      Height = 21
      MaxValue = 50.000000000000000000
      Value = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object btn_BorderColor: TJvColorButton
      Left = 101
      Top = 84
      Width = 65
      OtherCaption = '&Weitere...'
      Options = []
      TabOrder = 4
      TabStop = False
    end
    object cb_Border3D: TCheckBox
      Left = 176
      Top = 86
      Width = 81
      Height = 17
      Caption = '3D Rahmen'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
  end
  object pan_Top: TPanel
    Left = 0
    Top = 0
    Width = 268
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoBk
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    object Label11: TLabel
      Left = 10
      Top = 7
      Width = 108
      Height = 19
      Caption = 'Neue Tabelle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clInfoBk
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Image1: TImage
      Left = 234
      Top = 0
      Width = 24
      Height = 35
      Align = alRight
      AutoSize = True
      Center = True
      Picture.Data = {
        07544269746D6170F6060000424DF60600000000000036000000280000001800
        0000180000000100180000000000C0060000120B0000120B0000000000000000
        0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC6701CC6701CC6701CC6701CC67
        01CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC
        6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701FFF8F1
        FFF6ECFFF3E6FFF0DFFEEDD9993300FEE7CCFEE4C6FFE1C0FFDFBAFEDCB59933
        00FED8ADFED7ABFED7ABFED7ABFED7AB993300FED7ABFED7ABFED7ABFED7ABCC
        6701CC6701FFFCF7FFF9F2FEF7EDFFF3E7FEF0E0993300FEEBD3FEE7CDFEE4C7
        FEE1C1FEDFBB993300FEDAB2FED8AEFED7ABFED7ABFED7AB993300FED7ABFED7
        ABFED7ABFED7ABCC6701CC6701FFFDFBFFFBF7FFF9F3FFF6EDFFF4E7993300FF
        EEDBFEEBD5FEE8CEFFE5C7FFE2C2993300FEDCB7FEDAB2FED8AEFED7ABFED7AB
        993300FED7ABFED7ABFED7ABFED7ABCC6701CC6701FFFFFFFFFEFCFFFCF8FFF9
        F3FFF7EE993300FFF1E2FFEEDCFEEBD6FEE8CFFEE5C9993300FEDFBDFEDCB7FE
        DBB3FED9AEFED7AC993300FED7ABFED7ABFED7ABFED7ABCC6701CC6701993300
        9933009933009933009933009933009933009933009933009933009933009933
        00993300993300993300993300993300993300993300993300993300993300CC
        6701CC6701FFFFFFFFFFFFFFFFFFFFFEFDFFFCF9993300FFF8F0FFF5EAFEF3E4
        FFEFDDFFEDD7993300FFE6CBFFE3C5FEE0BFFFDEB9FEDBB4993300FED8ACFED7
        ABFED7ABFED7ABCC6701CC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFE993300FF
        FAF5FEF8F1FEF6EBFFF3E5FFEFDF993300FEEAD1FFE7CCFFE4C5FFE0BFFEDEBA
        993300FED9B0FED8ADFED7ABFED7ABCC6701CC6701FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF993300FFFDFAFFFBF7FFF9F2FFF6ECFFF3E6993300FEEDD9FFEAD2FE
        E6CCFEE3C6FEE1C0993300FEDCB5FEDAB1FED8ADFED8ADCC6701CC6701FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF993300FFFFFEFFFDFBFFFBF7FFF9F2FEF6EC9933
        00FEF0E0FFEDDAFEEBD4FFE8CDFFE4C7993300FFDFBCFEDCB6FEDBB2FEDBB2CC
        6701CC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF993300FFFFFFFFFFFEFFFEFB
        FFFBF8FFF9F3993300FEF4E7FEF1E2FFEEDBFEEBD4FEE7CE993300FEE1C1FEDF
        BBFEDCB6FEDCB6CC6701CC670199330099330099330099330099330099330099
        3300993300993300993300993300993300993300993300993300993300993300
        993300993300993300993300993300CC6701CC6701FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF993300FFFFFFFFFFFFFFFFFFFFFFFFFFFEFC993300FFF9F4FFF7EFFF
        F4EAFEF2E3FEEFDC993300FEE9D0FFE5C9FFE2C3FFE2C3CC6701CC6701FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF993300FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9933
        00FFFDF9FFFAF4FFF8EFFFF5EAFFF2E4993300FEECD8FEE9D1FEE6CBFEE6CBCC
        6701CC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF993300FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF993300FFFFFEFFFCFAFFFAF6FFF8F0FFF5EB993300FEF0DEFEED
        D8FFE9D1FFE9D1CC6701CC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF993300FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF993300FFFFFFFFFEFEFFFCFAFFFBF6FFF9F1
        993300FFF3E6FFF0E0FFEDD9FFEDD9CC6701CC6701E27E03E27E03E27E03E27E
        03E27E03E27E03E27E03E27E03E27E03E27E03E27E03E27E03E27E03E27E03E2
        7E03E27E03E27E03E27E03E27E03E27E03E27E03E27E03CC6701CC6701CC6701
        CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC67
        01CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC
        6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701
        CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC67
        01CC6701CC6701CC6701FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
      ExplicitLeft = 167
    end
    object pan_RightDelim: TPanel
      Left = 258
      Top = 0
      Width = 10
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
    end
  end
end
