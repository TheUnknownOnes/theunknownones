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
    Top = 326
    Width = 593
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Layout = tlCenter
    WordWrap = True
    ExplicitTop = 312
    ExplicitWidth = 3
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
  object tm_Aligner: TTimer
    Interval = 300
    OnTimer = tm_AlignerTimer
    Left = 456
    Top = 80
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
