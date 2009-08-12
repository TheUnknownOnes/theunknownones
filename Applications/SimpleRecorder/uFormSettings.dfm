object form_Settings: Tform_Settings
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Settings'
  ClientHeight = 254
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollMax: TJvScrollMax
    Left = 0
    Top = 0
    Width = 480
    Height = 254
    ButtonFont.Charset = DEFAULT_CHARSET
    ButtonFont.Color = clWindowText
    ButtonFont.Height = -9
    ButtonFont.Name = 'Small Fonts'
    ButtonFont.Style = []
    AutoHeight = False
    Align = alClient
    ParentColor = True
    TabOrder = 0
    ExplicitTop = -8
    ExplicitHeight = 308
    object bnd_Waveview: TJvScrollMaxBand
      Width = 462
      Height = 100
      Caption = 'Waveview'
      ExpandedHeight = 100
      object GridPanel1: TGridPanel
        Left = 3
        Top = 20
        Width = 456
        Height = 77
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 100.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Label1
            Row = 0
          end
          item
            Column = 1
            Control = track_WaveInterval
            Row = 0
          end
          item
            Column = 0
            Control = Label2
            Row = 1
          end
          item
            Column = 1
            Control = track_WaveZoom
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        ExplicitLeft = 136
        ExplicitTop = 32
        ExplicitWidth = 185
        ExplicitHeight = 41
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 100
          Height = 38
          Align = alClient
          Alignment = taRightJustify
          Caption = 'Refreshinterval:'
          Layout = tlCenter
          ExplicitWidth = 78
          ExplicitHeight = 13
        end
        object track_WaveInterval: TJvTrackBar
          Left = 100
          Top = 0
          Width = 356
          Height = 38
          Align = alClient
          Max = 1000
          Min = 50
          Frequency = 20
          Position = 100
          ShowSelRange = False
          TabOrder = 0
          OnChange = track_WaveIntervalChange
          ExplicitLeft = 84
          ExplicitWidth = 372
          ExplicitHeight = 23
        end
        object Label2: TLabel
          Left = 0
          Top = 38
          Width = 100
          Height = 39
          Align = alClient
          Alignment = taRightJustify
          Caption = 'Zoom:'
          Layout = tlCenter
          ExplicitLeft = -6
          ExplicitTop = 35
        end
        object track_WaveZoom: TJvTrackBar
          Left = 100
          Top = 38
          Width = 356
          Height = 39
          Align = alClient
          Max = 1000
          Min = 5
          Frequency = 5
          Position = 100
          ShowSelRange = False
          TabOrder = 1
          OnChange = track_WaveZoomChange
          ExplicitLeft = 106
          ExplicitTop = 35
        end
      end
    end
    object bnd_AutoLevel: TJvScrollMaxBand
      Width = 462
      Height = 140
      Caption = 'Autolevel'
      ExpandedHeight = 140
      object GridPanel2: TGridPanel
        Left = 3
        Top = 20
        Width = 456
        Height = 117
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 100.000000000000000000
          end
          item
            Value = 60.753164104796760000
          end
          item
            Value = 39.246835895203240000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Label3
            Row = 0
          end
          item
            Column = 1
            Control = Label4
            Row = 0
          end
          item
            Column = 2
            Control = Label5
            Row = 0
          end
          item
            Column = 0
            Control = Label6
            Row = 1
          end
          item
            Column = 1
            Control = track_Max
            Row = 1
          end
          item
            Column = 2
            Control = ed_MaxInterval
            Row = 1
          end
          item
            Column = 0
            Control = Label7
            Row = 2
          end
          item
            Column = 1
            Control = track_Min
            Row = 2
          end
          item
            Column = 2
            Control = ed_MinInterval
            Row = 2
          end
          item
            Column = 0
            Control = Label8
            Row = 3
          end
          item
            Column = 1
            Control = track_NoAct
            Row = 3
          end>
        RowCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 17.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 32.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 32.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 32.000000000000000000
          end>
        TabOrder = 0
        ExplicitHeight = 77
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 100
          Height = 17
          Align = alClient
          ExplicitWidth = 3
          ExplicitHeight = 13
        end
        object Label4: TLabel
          Left = 100
          Top = 0
          Width = 216
          Height = 17
          Align = alClient
          Alignment = taCenter
          Caption = 'Value (%)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 57
          ExplicitHeight = 13
        end
        object Label5: TLabel
          Left = 316
          Top = 0
          Width = 140
          Height = 17
          Align = alClient
          Alignment = taCenter
          Caption = 'Checkinterval (ms)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitLeft = 278
          ExplicitWidth = 108
          ExplicitHeight = 13
        end
        object Label6: TLabel
          Left = 0
          Top = 17
          Width = 100
          Height = 32
          Align = alClient
          Alignment = taRightJustify
          Caption = 'Max. Level:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitLeft = 37
          ExplicitWidth = 63
          ExplicitHeight = 13
        end
        object track_Max: TJvTrackBar
          Left = 100
          Top = 17
          Width = 216
          Height = 32
          Align = alClient
          Max = 100
          ShowSelRange = False
          TabOrder = 0
          OnChange = track_MaxChange
          ExplicitLeft = 94
          ExplicitTop = 36
          ExplicitWidth = 178
          ExplicitHeight = 45
        end
        object ed_MaxInterval: TJvSpinEdit
          AlignWithMargins = True
          Left = 319
          Top = 22
          Width = 134
          Height = 22
          Margins.Top = 5
          Margins.Bottom = 5
          Align = alClient
          MaxValue = 10000.000000000000000000
          MinValue = 20.000000000000000000
          Value = 50.000000000000000000
          TabOrder = 1
          OnChange = ed_MaxIntervalChange
          ExplicitLeft = 168
          ExplicitTop = 56
          ExplicitWidth = 121
          ExplicitHeight = 21
        end
        object Label7: TLabel
          Left = 0
          Top = 49
          Width = 100
          Height = 32
          Align = alClient
          Alignment = taRightJustify
          Caption = 'Min. Level:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitLeft = 41
          ExplicitWidth = 59
          ExplicitHeight = 13
        end
        object track_Min: TJvTrackBar
          Left = 100
          Top = 49
          Width = 216
          Height = 32
          Align = alClient
          Max = 100
          ShowSelRange = False
          TabOrder = 2
          OnChange = track_MinChange
          ExplicitLeft = 97
          ExplicitTop = 52
        end
        object ed_MinInterval: TJvSpinEdit
          AlignWithMargins = True
          Left = 319
          Top = 54
          Width = 134
          Height = 22
          Margins.Top = 5
          Margins.Bottom = 5
          Align = alClient
          MaxValue = 10000.000000000000000000
          MinValue = 20.000000000000000000
          Value = 1000.000000000000000000
          TabOrder = 3
          OnChange = ed_MinIntervalChange
          ExplicitLeft = 168
          ExplicitTop = 56
          ExplicitWidth = 121
          ExplicitHeight = 21
        end
        object Label8: TLabel
          Left = 0
          Top = 81
          Width = 100
          Height = 32
          Align = alClient
          Alignment = taRightJustify
          Caption = 'No Action under:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          WordWrap = True
          ExplicitLeft = 8
          ExplicitWidth = 92
          ExplicitHeight = 13
        end
        object track_NoAct: TJvTrackBar
          Left = 100
          Top = 81
          Width = 216
          Height = 32
          Align = alClient
          Max = 100
          ShowSelRange = False
          TabOrder = 4
          OnChange = track_NoActChange
          ExplicitLeft = 94
          ExplicitTop = 36
          ExplicitWidth = 150
          ExplicitHeight = 45
        end
      end
    end
  end
end
