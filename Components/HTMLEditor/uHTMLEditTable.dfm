object form_ITEditTable: Tform_ITEditTable
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Tabelle bearbeiten'
  ClientHeight = 300
  ClientWidth = 281
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
  object gb_Border: TGroupBox
    Left = 0
    Top = 92
    Width = 281
    Height = 161
    Align = alTop
    Caption = 'Rahmen'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Breite:'
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
      Width = 32
      Height = 13
      Caption = 'Farbe:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 9
      Top = 88
      Width = 86
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
    object Label4: TLabel
      Left = 9
      Top = 123
      Width = 86
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
    object Label7: TLabel
      Left = 182
      Top = 96
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
    object Label8: TLabel
      Left = 182
      Top = 129
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
      Left = 140
      Top = 26
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
    object ed_BorderWidth: TJvSpinEdit
      Left = 63
      Top = 21
      Width = 73
      Height = 21
      MaxValue = 50.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object btn_BorderColor: TJvColorButton
      Left = 63
      Top = 51
      Width = 73
      OtherCaption = '&Weitere...'
      Options = [cdAnyColor]
      TabOrder = 1
      TabStop = False
    end
    object ed_CellPadding: TJvSpinEdit
      Left = 103
      Top = 91
      Width = 73
      Height = 21
      MaxValue = 50.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object ed_CellSpacing: TJvSpinEdit
      Left = 103
      Top = 124
      Width = 73
      Height = 21
      MaxValue = 50.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object cb_Border3D: TCheckBox
      Left = 144
      Top = 53
      Width = 81
      Height = 17
      Caption = '3D Rahmen'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object gb_TableSize: TGroupBox
    Left = 0
    Top = 35
    Width = 281
    Height = 57
    Align = alTop
    Caption = 'Gr'#246#223'e'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label5: TLabel
      Left = 9
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Breite:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 132
      Top = 26
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
    object ed_Width: TJvSpinEdit
      Left = 56
      Top = 21
      Width = 73
      Height = 21
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 266
    Width = 281
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      281
      34)
    object btn_OK: TBitBtn
      Left = 97
      Top = 7
      Width = 89
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TBitBtn
      Left = 192
      Top = 7
      Width = 89
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pan_Top: TPanel
    Left = 0
    Top = 0
    Width = 281
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
    object Image1: TImage
      Left = 247
      Top = 0
      Width = 24
      Height = 35
      Align = alRight
      AutoSize = True
      Center = True
      Picture.Data = {
        07544269746D6170F6060000424DF60600000000000036000000280000001800
        0000180000000100180000000000C0060000C30E0000C30E0000000000000000
        0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF636B6B636B6B636B6B636B6B63
        6B6B636B6B636B6B636B6B636B6B636B6B636B6B636B6B636B6B636B6B636B6B
        636B6B636B6BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8C8C8CFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF8C8C8CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF949494FFFFFFFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB5
        6BFFB56BFFB56BFFB56BFFB56BFFB56BFFFFFF949494FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF949494FFFFFFFFB56B00FFFF00FFFFFFB56B00FFFF
        00FFFFFFB56B00FFFF00FFFFFFB56B00FFFF00FFFFFFB56BFFFFFF949494FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494FFFFFFFFB56B00FFFF00
        FFFFFFB56B00FFFF00FFFFFFB56B00FFFF00FFFFFFB56B00FFFF00FFFFFFB56B
        FFFFFF949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494FFFF
        FFFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFFB56BFF
        B56BFFB56BFFB56BFFFFFF949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF949494FFFFFFF7A55A00FFFF00FFFFF7A55A00FFFF00FFFFF7A55A00FF
        FF00FFFFF7A55A00FFFF00FFFFF7A55AFFFFFF949494FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF9C9C94FFFFFFF7A55A00FFFF00FFFFF7A55A00FFFF
        00FFFFF7A55A00FFFF00FFFFF7A55A00FFFF00FFFFF7A55AFFFFFF9C9C94FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9C9C9CFFFFFFF7A55AF7A55AF7
        A55AF7A55AF7A55AF7A55AF7A55AF7A55AF7A55AF7A55AF7A55AF7A55AF7A55A
        FFFFFF9C9C9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5A5A5FFFF
        FFEF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100
        FFFF00FFFFEF9C21FFFFFFA5A5A5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFA5A5A5F7F7F7EF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100FF
        FF00FFFFEF9C2100FFFF00FFFFEF9C21F7F7F7A5A5A5FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFA5A5A5F7F7F7EF9C21EF9C21EF9C21EF9C21EF9C21
        EF9C21EF9C21EF9C21EF9C21EF9C21EF9C21EF9C21EF9C21F7F7F7A5A5A5FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADEFEFEFEF9C2100FFFF00
        FFFFEF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C21
        EFEFEFADADADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADE7E7
        E7EF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100FFFF00FFFFEF9C2100
        FFFF00FFFFEF9C21E7E7E7ADADADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFADADADE7E7E7EF9C21EF9C21EF9C21EF9C21EF9C21EF9C21EF9C21EF9C
        21EF9C21EF9C21EF9C21EF9C21EF9C21E7E7E7ADADADFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFADB5B5DEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDE
        DEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEDEADB5B5FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B5D6D6D6D6D6D6D6D6D6D6
        D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6
        D6D6D6B5B5B5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5B5B5D6D6
        D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6
        D6D6D6D6D6D6D6D6D6D6D6B5B5B5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFB5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5
        B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
      ExplicitHeight = 24
    end
    object Label10: TLabel
      Left = 10
      Top = 8
      Width = 185
      Height = 19
      Caption = 'Tabelleneigenschaften'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clInfoBk
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object pan_RightDelim: TPanel
      Left = 271
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
