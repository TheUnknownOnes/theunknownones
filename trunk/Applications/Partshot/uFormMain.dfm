object form_Main: Tform_Main
  Left = 0
  Top = 0
  Caption = 'Partshot'
  ClientHeight = 429
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gb_ActionAfter: TGroupBox
    Left = 8
    Top = 8
    Width = 409
    Height = 153
    Caption = 'Action after shooting'
    TabOrder = 0
    object gb_SaveOptions: TGroupBox
      Left = 32
      Top = 40
      Width = 297
      Height = 104
      TabOrder = 1
      object Label1: TLabel
        Left = 32
        Top = 16
        Width = 34
        Height = 13
        Caption = 'Folder:'
      end
      object Label2: TLabel
        Left = 35
        Top = 43
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label3: TLabel
        Left = 23
        Top = 70
        Width = 43
        Height = 13
        Caption = 'Counter:'
      end
      object Label9: TLabel
        Left = 263
        Top = 43
        Width = 22
        Height = 13
        Caption = '.png'
      end
      object ed_SaveToFolder: TJvDirectoryEdit
        Left = 72
        Top = 13
        Width = 209
        Height = 21
        DialogKind = dkWin32
        TabOrder = 0
      end
      object ed_Format: TEdit
        Left = 72
        Top = 40
        Width = 185
        Height = 21
        TabOrder = 1
      end
      object ed_Counter: TSpinEdit
        Left = 72
        Top = 67
        Width = 65
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
    end
    object rb_SaveToFile: TRadioButton
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Save to folder'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rb_SaveToFileClick
    end
  end
  object gb_ShotSize: TGroupBox
    Left = 8
    Top = 167
    Width = 185
    Height = 221
    Caption = 'Shotsize'
    TabOrder = 1
    object rb_SizeFix: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Size'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rb_SizeFixClick
    end
    object gb_FixSizeOptions: TGroupBox
      Left = 32
      Top = 39
      Width = 137
      Height = 78
      TabOrder = 1
      object Label4: TLabel
        Left = 16
        Top = 19
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label5: TLabel
        Left = 13
        Top = 47
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object ed_FixSizeWidth: TSpinEdit
        Left = 54
        Top = 16
        Width = 59
        Height = 22
        MaxValue = 9999999
        MinValue = 32
        TabOrder = 0
        Value = 800
      end
      object ed_FixSizeHeight: TSpinEdit
        Left = 54
        Top = 44
        Width = 59
        Height = 22
        MaxValue = 9999999
        MinValue = 32
        TabOrder = 1
        Value = 600
      end
    end
    object rb_Ratio: TRadioButton
      Left = 16
      Top = 123
      Width = 113
      Height = 17
      Caption = 'Ratio'
      TabOrder = 2
      OnClick = rb_SizeFixClick
    end
    object gb_RatioOptions: TGroupBox
      Left = 32
      Top = 135
      Width = 137
      Height = 78
      TabOrder = 3
      object Label6: TLabel
        Left = 16
        Top = 19
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label7: TLabel
        Left = 13
        Top = 47
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object ed_RatioX: TSpinEdit
        Left = 54
        Top = 16
        Width = 59
        Height = 22
        MaxValue = 9999999
        MinValue = 1
        TabOrder = 0
        Value = 4
      end
      object ed_RatioY: TSpinEdit
        Left = 54
        Top = 44
        Width = 59
        Height = 22
        MaxValue = 9999999
        MinValue = 1
        TabOrder = 1
        Value = 3
      end
    end
  end
  object pan_Bottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 393
    Width = 426
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 348
      Top = 3
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'OK'
      TabOrder = 0
      OnClick = btn_OKClick
    end
  end
  object gb_Misc: TGroupBox
    Left = 199
    Top = 167
    Width = 218
    Height = 90
    Caption = 'Misc options'
    TabOrder = 3
    object Label8: TLabel
      Left = 16
      Top = 56
      Width = 65
      Height = 13
      Caption = 'Shootercolor:'
    end
    object cb_MultiShots: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Multiple shots'
      TabOrder = 0
    end
    object cl_Shooter: TJvColorButton
      Left = 87
      Top = 52
      OtherCaption = '&Other...'
      Options = []
      Color = clRed
      OnChange = cl_ShooterChange
      TabOrder = 1
      TabStop = False
    end
  end
  object Tray: TJvTrayIcon
    Active = True
    IconIndex = 0
    Hint = 'Click for shooting'
    PopupMenu = pum_tray
    Visibility = [tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide, tvRestoreDbClick]
    OnClick = TrayClick
    Left = 336
    Top = 24
  end
  object Settings: TSettingsXMLFile
    Left = 376
    Top = 24
  end
  object SettingsLinkComponent1: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ActionAfter/gb_SaveOptions/ed_SaveToFolder'
    SaveProperties.Strings = (
      'Text')
    Settings = Settings
    Component = ed_SaveToFolder
    Left = 200
    Top = 56
  end
  object SettingsLinkComponent2: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ActionAfter/gb_SaveOptions/ed_Format'
    SaveProperties.Strings = (
      'Text')
    Settings = Settings
    Component = ed_Format
    Left = 240
    Top = 80
  end
  object SettingsLinkComponent3: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/gb_FixSizeOptions/ed_FixSizeWidth'
    SaveProperties.Strings = (
      'Value')
    Settings = Settings
    Component = ed_FixSizeWidth
    Left = 128
    Top = 208
  end
  object SettingsLinkComponent4: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/gb_FixSizeOptions/ed_FixSizeHeight'
    SaveProperties.Strings = (
      'Value')
    Settings = Settings
    Component = ed_FixSizeHeight
    Left = 120
    Top = 240
  end
  object SettingsLinkComponent5: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/gb_RatioOptions/ed_RatioX'
    SaveProperties.Strings = (
      'Value')
    Settings = Settings
    Component = ed_RatioX
    Left = 112
    Top = 304
  end
  object SettingsLinkComponent6: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/gb_RatioOptions/ed_RatioY'
    SaveProperties.Strings = (
      'Value')
    Settings = Settings
    Component = ed_RatioY
    Left = 104
    Top = 336
  end
  object SettingsLinkComponent7: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/rb_SizeFix'
    SaveProperties.Strings = (
      'Checked')
    Settings = Settings
    Component = rb_SizeFix
    Left = 88
    Top = 184
  end
  object SettingsLinkComponent8: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_ShotSize/rb_Ratio'
    SaveProperties.Strings = (
      'Checked')
    Settings = Settings
    Component = rb_Ratio
    Left = 64
    Top = 280
  end
  object SettingsLinkComponent9: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_Misc/cb_MultiShots'
    SaveProperties.Strings = (
      'Checked')
    Settings = Settings
    Component = cb_MultiShots
    Left = 304
    Top = 184
  end
  object pum_tray: TPopupMenu
    Left = 336
    Top = 56
    object mi_Config: TMenuItem
      Caption = 'Configure'
      Default = True
      OnClick = mi_ConfigClick
    end
    object mi_Exit: TMenuItem
      Caption = 'Exit'
      OnClick = mi_ExitClick
    end
  end
  object SettingsLinkComponent10: TSettingsLinkComponent
    DefaultRootSetting = '/GUI/form_Main/gb_Misc/cl_Shooter'
    SaveProperties.Strings = (
      'Color')
    Settings = Settings
    Component = cl_Shooter
    Left = 344
    Top = 216
  end
end
