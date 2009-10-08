object FormResEdManifestEditor: TFormResEdManifestEditor
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'FormResEdManifestEditor'
  ClientHeight = 160
  ClientWidth = 459
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
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 82
    Height = 13
    Caption = 'Application Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 108
    Height = 13
    Caption = 'Application Description'
  end
  object Label3: TLabel
    Left = 8
    Top = 83
    Width = 130
    Height = 13
    Caption = 'Requested Execution Level'
  end
  object edAppName: TEdit
    Left = 144
    Top = 8
    Width = 201
    Height = 21
    TabOrder = 0
  end
  object edAppDesc: TEdit
    Left = 144
    Top = 35
    Width = 305
    Height = 21
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 370
    Top = 128
    Width = 75
    Height = 25
    Caption = '&Save'
    ModalResult = 1
    TabOrder = 2
  end
  object cbUIA: TCheckBox
    Left = 144
    Top = 107
    Width = 97
    Height = 17
    Caption = 'UI Access'
    TabOrder = 3
  end
  object cbREL: TComboBox
    Left = 144
    Top = 80
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'asInvoker'
    Items.Strings = (
      'asInvoker'
      'highestAvailable'
      'requireAdministrator')
  end
end
