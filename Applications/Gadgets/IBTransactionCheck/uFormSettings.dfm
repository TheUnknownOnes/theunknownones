object form_Settings: Tform_Settings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'IBTransactionChecker Settings'
  ClientHeight = 260
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Database:'
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 52
    Height = 13
    Caption = 'Username:'
  end
  object Label3: TLabel
    Left = 8
    Top = 62
    Width = 55
    Height = 13
    Caption = 'Passsword:'
  end
  object Label4: TLabel
    Left = 8
    Top = 115
    Width = 42
    Height = 13
    Caption = 'Charset:'
  end
  object Label5: TLabel
    Left = 8
    Top = 142
    Width = 61
    Height = 13
    Caption = 'Clientlibrary:'
  end
  object Label6: TLabel
    Left = 8
    Top = 89
    Width = 25
    Height = 13
    Caption = 'Role:'
  end
  object Label7: TLabel
    Left = 8
    Top = 169
    Width = 114
    Height = 13
    Caption = 'Min. transaction length:'
  end
  object Label8: TLabel
    Left = 199
    Top = 169
    Width = 45
    Height = 13
    Caption = 'minute(s)'
  end
  object Label9: TLabel
    Left = 8
    Top = 197
    Width = 78
    Height = 13
    Caption = 'Refreshinterval:'
  end
  object Label10: TLabel
    Left = 199
    Top = 197
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object Panel1: TPanel
    Left = 0
    Top = 229
    Width = 316
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitTop = 164
    ExplicitWidth = 319
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 157
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
      ExplicitLeft = 160
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 238
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 241
    end
  end
  object ed_DB: TEdit
    Left = 80
    Top = 5
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object ed_Username: TEdit
    Left = 80
    Top = 32
    Width = 233
    Height = 21
    TabOrder = 1
  end
  object ed_Password: TEdit
    Left = 80
    Top = 59
    Width = 233
    Height = 21
    PasswordChar = '#'
    TabOrder = 2
  end
  object ed_Charset: TEdit
    Left = 80
    Top = 112
    Width = 233
    Height = 21
    TabOrder = 4
  end
  object ed_ClientDLL: TJvFilenameEdit
    Left = 80
    Top = 139
    Width = 233
    Height = 21
    Filter = 'Libraries (*.dll)|*.dll'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    TabOrder = 5
  end
  object ed_Role: TEdit
    Left = 80
    Top = 86
    Width = 233
    Height = 21
    PasswordChar = '#'
    TabOrder = 3
  end
  object ed_MinLen: TSpinEdit
    Left = 128
    Top = 166
    Width = 65
    Height = 22
    MaxValue = 65000
    MinValue = 1
    TabOrder = 7
    Value = 1
  end
  object ed_Refresh: TSpinEdit
    Left = 128
    Top = 194
    Width = 65
    Height = 22
    MaxValue = 2147483647
    MinValue = 10000
    TabOrder = 8
    Value = 60000
  end
  object SettingsLinkForm: TSettingsLinkForm
    DefaultRootSetting = '/GUI/form_Settings'
    SaveProperties.Strings = (
      'Left'
      'Top')
    Settings = Data.SettingsFile
    Left = 280
    Top = 8
  end
end
