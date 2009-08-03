object form_Settings: Tform_Settings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'IBTransactionChecker Settings'
  ClientHeight = 195
  ClientWidth = 319
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
  object Panel1: TPanel
    Left = 0
    Top = 164
    Width = 319
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitTop = 198
    ExplicitWidth = 381
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 160
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
      ExplicitLeft = 80
      ExplicitTop = 8
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 241
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 152
      ExplicitTop = 8
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
  object SettingsLinkForm: TSettingsLinkForm
    DefaultRootSetting = '/GUI/form_Settings'
    SaveProperties.Strings = (
      'Height'
      'Left'
      'Top'
      'Width')
    Settings = Data.SettingsFile
    Left = 336
    Top = 40
  end
end
