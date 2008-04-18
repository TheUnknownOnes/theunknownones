object Data: TData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 357
  Width = 655
  object Tray: TJvTrayIcon
    Active = True
    IconIndex = 0
    PopupMenu = pum_Tray
    Visibility = [tvVisibleTaskList, tvAutoHide]
    OnDblClick = TrayDblClick
    Left = 16
    Top = 16
  end
  object pum_Tray: TPopupMenu
    Left = 56
    Top = 16
    object mi_Close: TMenuItem
      Caption = 'Beenden'
      OnClick = mi_CloseClick
    end
  end
end
