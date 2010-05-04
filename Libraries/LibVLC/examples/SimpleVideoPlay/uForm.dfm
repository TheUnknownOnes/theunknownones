object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'SimpleVideoPlay'
  ClientHeight = 337
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 304
    Width = 527
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_OpenFile: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = 'Open Videofile'
      TabOrder = 0
      OnClick = btn_OpenFileClick
      ExplicitHeight = 26
    end
    object btn_Play: TButton
      AlignWithMargins = True
      Left = 84
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = 'Play'
      TabOrder = 1
      OnClick = btn_PlayClick
      ExplicitHeight = 26
    end
    object btn_Pause: TButton
      AlignWithMargins = True
      Left = 165
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = 'Pause'
      TabOrder = 2
      OnClick = btn_PauseClick
      ExplicitHeight = 26
    end
    object btn_Stop: TButton
      AlignWithMargins = True
      Left = 246
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = 'Stop'
      TabOrder = 3
      OnClick = btn_StopClick
      ExplicitHeight = 26
    end
  end
  object pan_Video: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 273
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    ExplicitHeight = 305
  end
  object track: TTrackBar
    Left = 0
    Top = 273
    Width = 527
    Height = 31
    Align = alBottom
    Max = 1
    ShowSelRange = False
    TabOrder = 2
    OnChange = trackChange
  end
  object dlg_Video: TOpenDialog
    Filter = 'Movie-Files|*.avi;*.mpeg'
    Left = 256
    Top = 176
  end
  object tm_RefreshTrack: TTimer
    OnTimer = tm_RefreshTrackTimer
    Left = 376
    Top = 184
  end
end
