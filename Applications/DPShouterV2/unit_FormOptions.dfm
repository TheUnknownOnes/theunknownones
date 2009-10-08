object form_Options: Tform_Options
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Optionen'
  ClientHeight = 371
  ClientWidth = 292
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
  object gb_Alpha: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 185
    Caption = 'Transparenzen'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 43
      Height = 26
      Caption = 'Aktiver Zustand:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label2: TLabel
      Left = 70
      Top = 52
      Width = 24
      Height = 13
      Caption = 'mehr'
    end
    object Label3: TLabel
      Left = 232
      Top = 52
      Width = 38
      Height = 13
      Caption = 'weniger'
    end
    object Label4: TLabel
      Left = 16
      Top = 75
      Width = 43
      Height = 26
      Caption = 'Inaktiver Zustand:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label5: TLabel
      Left = 16
      Top = 126
      Width = 79
      Height = 26
      Caption = 'Fading- geschwindigkeit:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label6: TLabel
      Left = 101
      Top = 158
      Width = 49
      Height = 13
      Caption = 'langsamer'
    end
    object Label7: TLabel
      Left = 228
      Top = 158
      Width = 42
      Height = 13
      Caption = 'schneller'
    end
    object tb_AlphaActive: TTrackBar
      Left = 70
      Top = 24
      Width = 201
      Height = 30
      Max = 255
      Min = 5
      Frequency = 5
      Position = 200
      ShowSelRange = False
      TabOrder = 0
    end
    object tb_AlphaInactive: TTrackBar
      Left = 70
      Top = 75
      Width = 201
      Height = 45
      Max = 255
      Min = 5
      Frequency = 5
      Position = 150
      ShowSelRange = False
      TabOrder = 1
    end
    object tb_Fading: TTrackBar
      Left = 101
      Top = 126
      Width = 169
      Height = 35
      Max = 50
      Min = 5
      Frequency = 5
      Position = 20
      ShowSelRange = False
      TabOrder = 2
    end
  end
  object gb_Optionen: TGroupBox
    Left = 8
    Top = 199
    Width = 281
    Height = 98
    Caption = 'Optionen'
    TabOrder = 1
    object cb_ShowNewPosts: TCheckBox
      Left = 16
      Top = 24
      Width = 145
      Height = 17
      Caption = 'Neue Shouts anzeigen'
      TabOrder = 0
    end
    object cb_StayOnTop: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Stay On Top'
      TabOrder = 1
    end
    object cb_HighlightMe: TCheckBox
      Left = 16
      Top = 71
      Width = 97
      Height = 17
      Caption = '/me ersetzen'
      TabOrder = 2
    end
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 330
    Width = 292
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 341
    object btn_Ok: TBitBtn
      Left = 88
      Top = 9
      Width = 96
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TBitBtn
      Left = 190
      Top = 9
      Width = 99
      Height = 25
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object btn_DelCache: TBitBtn
    Left = 184
    Top = 303
    Width = 100
    Height = 25
    Caption = 'Cache l'#246'schen'
    TabOrder = 3
    OnClick = btn_DelCacheClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FF01079F0313A90418AE0419AE0313A90108A0FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF01049D041CB10730C00734C407
      35C50735C50734C30731C1041FB301069EFF00FFFF00FFFF00FFFF00FFFF00FF
      0109A1052BC30735C70733C20732C20732C20732C20732C20733C30735C4062D
      BE020CA4FF00FFFF00FFFF00FF01049B052BCA0636D80431CD0027C4032EC107
      32C20732C20430C10027BF042FC10735C4072EBE01069EFF00FFFF00FF031ABA
      0537E70331DD123DD86480E01840CB002CC1022DC00F38C46580D91B43C7052F
      C10735C5051FB3FF00FF01049E0430E40436F1002AE45070E9FFFFFFB7C4F10D
      36CA042DC3A2B2E8FFFFFF6984DA0026BE0733C30731C10108A0020FAF0336FA
      0335F80232EE0A35E88CA2F2FFFFFFB4C2F1A9B8EDFFFFFFA7B7E9133AC4052F
      C10732C20734C40313AA0619BC1747FE093AFC0435F80131F0002BE891A5F4FF
      FFFFFFFFFFABBAEF062FC5022DC00732C20732C20736C50419AE0B1DBE4168FE
      1C49FC0335FB0031F90531F2A4B5F7FFFFFFFFFFFFB9C6F20D36D0002CC60732
      C20732C20736C50418AD0613B45B7CFC486CFD0133FB113CFBA1B4FEFFFFFFA4
      B6F892A7F5FFFFFFB6C4F21A41D3042FC80732C40734C30212A90003A04A6AF3
      8FA6FF1F46FB4C6FFCFFFFFFA7B8FE0733F6002AED8CA2F6FFFFFF627FE70028
      D00734CC0730C300069FFF00FF1A2FCB99AFFF8BA2FE214DFB4D71FC0E3DFB00
      30FB0031F70636F14C6EF1103CE30432DB0636D7041CB5FF00FFFF00FF0004A0
      415EECB8C7FF9CAFFD3A5CFC0A3AFB0335FB0335FB0133F9052FF20635EB0537
      E9052CCD00049CFF00FFFF00FFFF00FF0309A54260ECA9BBFFBDCAFF8EA5FE64
      83FD5073FC4A6EFD3961FD1444F9042CD70109A2FF00FFFF00FFFF00FFFF00FF
      FF00FF0004A01E32CD5876F6859EFE8BA3FF7994FE5376FC234AF0051EC50104
      9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0004A00917B610
      22C30D1FC20311B401059FFF00FFFF00FFFF00FFFF00FFFF00FF}
  end
end
