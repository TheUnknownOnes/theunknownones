object CNA2ConfigDialog: TCNA2ConfigDialog
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'CNA2-Config'
  ClientHeight = 433
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 716
    Height = 433
    ActivePage = ts_ComponentSettings
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    object ts_ComponentSettings: TTabSheet
      Caption = 'Component-Settings'
      DesignSize = (
        708
        402)
      object TreeView1: TTreeView
        Left = 3
        Top = 3
        Width = 302
        Height = 396
        Anchors = [akLeft, akTop, akBottom]
        Indent = 19
        TabOrder = 0
      end
    end
  end
end
