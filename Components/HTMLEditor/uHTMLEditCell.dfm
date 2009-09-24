object form_ITEditCell: Tform_ITEditCell
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Zelle bearbeiten'
  ClientHeight = 189
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gb_Color: TGroupBox
    Left = 166
    Top = 38
    Width = 161
    Height = 107
    Caption = 'Farben'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 13
      Top = 24
      Width = 61
      Height = 13
      Caption = 'Hintergrund:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 13
      Top = 51
      Width = 43
      Height = 13
      Caption = 'Rahmen:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btn_BGColor: TJvColorButton
      Left = 80
      Top = 20
      Width = 57
      OtherCaption = '&Other...'
      Options = [cdAnyColor]
      TabOrder = 0
      TabStop = False
    end
    object btn_BorderColor: TJvColorButton
      Left = 80
      Top = 47
      Width = 57
      OtherCaption = '&Other...'
      Options = [cdAnyColor]
      TabOrder = 1
      TabStop = False
    end
    object cb_Border3d: TCheckBox
      Left = 80
      Top = 74
      Width = 78
      Height = 17
      Caption = '3D Rahmen'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object gb_Span: TGroupBox
    Left = -1
    Top = 38
    Width = 161
    Height = 107
    Caption = 'Die Zelle geht '#252'ber ...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbl_Cols: TLabel
      Left = 106
      Top = 33
      Width = 30
      Height = 13
      Caption = 'Spalte'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbl_Rows: TLabel
      Left = 106
      Top = 60
      Width = 22
      Height = 13
      Caption = 'Zeile'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ed_ColSpan: TJvSpinEdit
      Left = 24
      Top = 28
      Width = 76
      Height = 22
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
      OnChange = ed_ColSpanChange
    end
    object ed_RowSpan: TJvSpinEdit
      Left = 24
      Top = 56
      Width = 76
      Height = 22
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = ed_RowSpanChange
    end
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 154
    Width = 329
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      329
      35)
    object btn_OK: TBitBtn
      Left = 120
      Top = 5
      Width = 97
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 122
    end
    object btn_Cancel: TBitBtn
      Left = 223
      Top = 5
      Width = 97
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 225
    end
  end
  object pan_Top: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoBk
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    ExplicitWidth = 331
    object Image1: TImage
      Left = 295
      Top = 0
      Width = 24
      Height = 24
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
        FF00FF949494FFFFFFFFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFF
        B7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFFF949494FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF949494FFFFFFFFFFB7FFFFFFFFFFFFFFFFB7FFFFFF
        FFFFFFFFFFB7FFFFFFFFFFFFFFFFB7FFFFFFFFFFFFFFFFB7FFFFFF949494FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494FFFFFFFFFFB7FFFFFFFF
        FFFFFFFFB7FFFFFFFFFFFFFFFFB7FFFFFFFFFFFFFFFFB7FFFFFFFFFFFFFFFFB7
        FFFFFF949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494FFFF
        FFFFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FFFFB7FF
        FFB7FFFFB7FFFFB7FFFFFF949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF949494FFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFF
        FFFFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFFFF949494FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF9C9C94FFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFFFF
        FFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFFFFFFFFFFFFF1A6FFFFFF9C9C94FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9C9C9CFFFFFFFFF1A6FFF1A6FF
        F1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6FFF1A6
        FFFFFF9C9C9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5A5A5FFFF
        FFFFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFFFE86DFF
        FFFFFFFFFFFFE86DFFFFFFA5A5A5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFA5A5A5F7F7F7FFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFFFE86DFFFF
        FFFFFFFFFFE86DFFFFFFFFFFFFFFE86DF7F7F7A5A5A5FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFA5A5A5F7F7F7FFE86DFFE86DFFE86DFFE86DFFE86D
        FFE86DFFE86DFFE86DFFE86DEF9C21EF9C21EF9C21EF9C21F7F7F7A5A5A5FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADEFEFEFFFE86DFFFFFFFF
        FFFFFFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFEF9C2100FFFF00FFFFEF9C21
        EFEFEFADADADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADE7E7
        E7FFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFFFE86DFFFFFFFFFFFFEF9C2100
        FFFF00FFFFEF9C21E7E7E7ADADADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFADADADE7E7E7FFE86DFFE86DFFE86DFFE86DFFE86DFFE86DFFE86DFFE8
        6DFFE86DEF9C21EF9C21EF9C21EF9C21E7E7E7ADADADFF00FFFF00FFFF00FFFF
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
      ExplicitLeft = 254
      ExplicitHeight = 33
    end
    object Label3: TLabel
      Left = 10
      Top = 7
      Width = 191
      Height = 19
      Caption = 'Eigenschaften der Zelle'
      Transparent = True
    end
    object pan_RightDelim: TPanel
      Left = 319
      Top = 0
      Width = 10
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      ExplicitLeft = 321
    end
  end
end
