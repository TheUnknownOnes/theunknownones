object form_Main: Tform_Main
  Left = 0
  Top = 0
  Caption = 'PodLoader'
  ClientHeight = 373
  ClientWidth = 594
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
  object Toolbar: TEffectPNGToolBar
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 588
    Height = 22
    TabOrder = 0
    ImageEffectDefault.GammaValue = 1.000000000000000000
    ImageEffectDisabled.GammaValue = 1.000000000000000000
    object btn_AddPodCast: TEffectPNGToolButton
      Left = 0
      Top = 0
      Hint = 'Add PodCast'
      ParentShowHint = False
      ShowHint = True
      OnClick = btn_AddPodCastClick
    end
  end
  object TV: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 31
    Width = 588
    Height = 339
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    TabOrder = 1
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnAfterCellPaint = TVAfterCellPaint
    OnGetText = TVGetText
    OnGetNodeDataSize = TVGetNodeDataSize
    Columns = <
      item
        Position = 0
        Width = 200
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 150
        WideText = 'State'
      end
      item
        Position = 2
        Width = 100
        WideText = 'Progress'
      end>
  end
  object SettingsLinkForm: TSettingsLinkForm
    DefaultRootSetting = '/GUI/form_Main'
    SaveProperties.Strings = (
      'Height'
      'Left'
      'Top'
      'Width'
      'WindowState')
    Settings = Data.Settings
    Left = 168
    Top = 104
  end
  object SettingsLinkVST: TSettingsLinkVST
    Settings = Data.Settings
    DefaultRootSetting = '/GUI/form_Main/VST'
    Tree = TV
    Left = 288
    Top = 112
  end
  object tm_Check: TTimer
    OnTimer = tm_CheckTimer
    Left = 288
    Top = 192
  end
end
