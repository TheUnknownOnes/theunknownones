object form_EditComponentPropertyList: Tform_EditComponentPropertyList
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Select properties to save'
  ClientHeight = 286
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pan_Bottom: TPanel
    Left = 0
    Top = 255
    Width = 321
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 162
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 243
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object tv_Properties: TTreeView
    Left = 0
    Top = 0
    Width = 321
    Height = 255
    Hint = 'Use context menu for advanced selecting'
    Align = alClient
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    ParentShowHint = False
    PopupMenu = pum_TV
    ReadOnly = True
    RowSelect = True
    ShowHint = True
    StateImages = iml_TV
    TabOrder = 1
    OnDeletion = tv_PropertiesDeletion
    OnKeyUp = tv_PropertiesKeyUp
    OnMouseUp = tv_PropertiesMouseUp
  end
  object pum_TV: TPopupMenu
    Left = 152
    Top = 144
    object mi_CheckAll: TMenuItem
      Caption = 'Check all'
      OnClick = mi_CheckAllClick
    end
    object mi_UncheckAll: TMenuItem
      Caption = 'Uncheck all'
      OnClick = mi_UncheckAllClick
    end
    object mi_InvertChecks: TMenuItem
      Caption = 'Invert checks'
      OnClick = mi_InvertChecksClick
    end
  end
  object iml_TV: TImageList
    Left = 144
    Top = 56
  end
end
