object ch2FormConfigStaticWebsearch: Tch2FormConfigStaticWebsearch
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure "Static Websearch"'
  ClientHeight = 371
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 340
    Width = 529
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 451
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 370
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 513
    Height = 121
    Caption = 'Properties'
    TabOrder = 1
    object Label1: TLabel
      Left = 45
      Top = 24
      Width = 38
      Height = 13
      Caption = 'Priority:'
    end
    object ed_Prio: TSpinEdit
      Left = 89
      Top = 21
      Width = 77
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object ColorBox1: TColorBox
      Left = 89
      Top = 47
      Width = 113
      Height = 22
      ItemHeight = 16
      TabOrder = 1
    end
    object ColorBox2: TColorBox
      Left = 294
      Top = 47
      Width = 113
      Height = 22
      ItemHeight = 16
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 11
      Top = 49
      Width = 72
      Height = 17
      Caption = 'ForeColor:'
      TabOrder = 3
    end
    object CheckBox2: TCheckBox
      Left = 216
      Top = 49
      Width = 72
      Height = 17
      Caption = 'BackColor:'
      TabOrder = 4
    end
  end
end
