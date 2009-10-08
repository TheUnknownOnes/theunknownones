object Data: TData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 227
  Width = 347
  object SettingsFile: TSettingsXMLFile
    Left = 192
    Top = 64
  end
  object IMAP: TIdIMAP4
    MaxLineAction = maException
    Left = 96
    Top = 64
  end
  object Timer: TTimer
    Interval = 60000
    OnTimer = TimerTimer
    Left = 96
    Top = 144
  end
end
