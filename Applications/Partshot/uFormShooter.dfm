object form_Shooter: Tform_Shooter
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 100
  BorderStyle = bsNone
  ClientHeight = 342
  ClientWidth = 599
  Color = clRed
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pum_Shooter
  OnCreate = FormCreate
  OnMouseLeave = FormMouseLeave
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_Size: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 593
    Height = 13
    Align = alTop
    ExplicitWidth = 3
  end
  object lbl_Bottom: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 304
    Width = 593
    Height = 35
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Layout = tlCenter
    WordWrap = True
  end
  object lbl_Client: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 593
    Height = 13
    Align = alTop
    Alignment = taCenter
    Caption = 'Left click to shoot / Right click  for menu'
    Layout = tlCenter
    ExplicitWidth = 190
  end
  object pum_Shooter: TPopupMenu
    Left = 312
    Top = 144
    object mi_EndShooting: TMenuItem
      Caption = 'End shooting'
      OnClick = mi_EndShootingClick
    end
    object mi_Transp: TMenuItem
      Caption = 'Transparency'
    end
  end
end
