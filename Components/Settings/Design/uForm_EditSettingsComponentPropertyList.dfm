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
  object lv_Properties: TListView
    Left = 0
    Top = 25
    Width = 321
    Height = 230
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'Propertyname'
        Width = 250
      end>
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnMouseUp = lv_PropertiesMouseUp
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 255
    Width = 321
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
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
      ExplicitLeft = 81
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
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 321
    Height = 25
    Caption = 'ToolBar1'
    Indent = 5
    TabOrder = 2
    object btn_CheckAll: TToolButton
      Left = 5
      Top = 0
      Hint = 'Check all'
      Caption = 'btn_CheckAll'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = btn_CheckAllClick
    end
    object btn_CheckNothing: TToolButton
      Left = 28
      Top = 0
      Hint = 'Check nothing'
      Caption = 'btn_CheckNothing'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = True
      OnClick = btn_CheckNothingClick
    end
    object btn_InvertChecks: TToolButton
      Left = 51
      Top = 0
      Hint = 'Invert checks'
      Caption = 'btn_InvertChecks'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = btn_InvertChecksClick
    end
  end
end
