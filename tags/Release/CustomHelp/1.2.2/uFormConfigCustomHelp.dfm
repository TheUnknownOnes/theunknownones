object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Custom Help'
  ClientHeight = 591
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Tabs: TPageControl
    Left = 0
    Top = 0
    Width = 664
    Height = 591
    ActivePage = TabSheet6
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 534
        Width = 626
        Height = 26
        Cursor = crHandPoint
        Align = alBottom
        Caption = 
          'go to the discussion thread at www.delphipraxis.net to receive h' +
          'elp or post provider URLs (German and English spoken)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
        WordWrap = True
        OnClick = Label1Click
      end
      object rgDisplayLocation: TRadioGroup
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 650
        Height = 182
        Align = alTop
        Caption = 'Where to display help?'
        TabOrder = 0
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 191
        Width = 650
        Height = 337
        Align = alClient
        Caption = 'Result order'
        TabOrder = 1
        object lbOrder: TListBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 640
          Height = 314
          Hint = 'Use drag and drop to rearrange items'
          Align = alClient
          DragMode = dmAutomatic
          ExtendedSelect = False
          ItemHeight = 13
          TabOrder = 0
          OnDragDrop = lbOrderDragDrop
          OnDragOver = lbOrderDragOver
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Microsoft Help 2.x'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvNamespaces: TListView
        AlignWithMargins = True
        Left = 3
        Top = 31
        Width = 650
        Height = 486
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Help namespace'
            Width = 200
          end
          item
            AutoSize = True
            Caption = 'Description'
          end
          item
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Panel4: TPanel
        Left = 0
        Top = 520
        Width = 656
        Height = 43
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label4: TLabel
          Left = 6
          Top = 22
          Width = 176
          Height = 13
          Caption = 'Trim namespaces until result is found'
        end
        object cbFullTextSearch: TCheckBox
          Left = 6
          Top = 3
          Width = 579
          Height = 17
          Caption = 
            'Perform fulltext search ... this may be slow (if unchecked only ' +
            'index searches are performed)'
          TabOrder = 0
        end
        object cbTrimNamespacesHX: TComboBox
          Left = 192
          Top = 19
          Width = 145
          Height = 21
          Hint = 
            'Example:'#13#10'Full Keyword is  Classes.TStringList.Create'#13#10'Trim Firs' +
            't searches for TStringList.Create'#13#10'Trim Full searches for Create'
          Style = csDropDownList
          ItemHeight = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
      end
      inline fccMSHelp: TFrameConfigColor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 650
        Height = 22
        Align = alTop
        AutoSize = True
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 650
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Static Web based providers'
      ImageIndex = 2
      inline FrameConfigWebBasedProviders: TFrameConfigProviders
        Left = 0
        Top = 28
        Width = 656
        Height = 535
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 28
        ExplicitWidth = 656
        ExplicitHeight = 535
        inherited pnlOHSItem: TPanel
          Top = 441
          Width = 656
          ExplicitTop = 441
          ExplicitWidth = 656
          inherited BtnBrowseForFile: TSpeedButton
            Left = 626
            ExplicitLeft = 626
          end
          inherited edName: TEdit
            Width = 558
            ExplicitWidth = 558
          end
          inherited edDesc: TEdit
            Width = 558
            ExplicitWidth = 558
          end
          inherited edURL: TEdit
            Width = 531
            ExplicitWidth = 531
          end
        end
        inherited ListView1: TListView
          Width = 650
          Height = 435
          ExplicitWidth = 650
          ExplicitHeight = 435
        end
      end
      inline fccWebProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 650
        Height = 22
        Align = alTop
        AutoSize = True
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 650
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'File based providers'
      ImageIndex = 3
      inline FrameConfigFileBasedProviders: TFrameConfigProviders
        Left = 0
        Top = 80
        Width = 656
        Height = 483
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 80
        ExplicitWidth = 656
        ExplicitHeight = 483
        inherited pnlOHSItem: TPanel
          Top = 389
          Width = 656
          ExplicitTop = 389
          ExplicitWidth = 656
          inherited BtnBrowseForFile: TSpeedButton
            Left = 626
            ExplicitLeft = 626
          end
          inherited edName: TEdit
            Width = 558
            ExplicitWidth = 558
          end
          inherited edDesc: TEdit
            Width = 558
            ExplicitWidth = 558
          end
          inherited edURL: TEdit
            Width = 531
            ExplicitWidth = 531
          end
        end
        inherited ListView1: TListView
          Width = 650
          Height = 383
          ExplicitWidth = 650
          ExplicitHeight = 383
        end
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 31
        Width = 650
        Height = 46
        Align = alTop
        Caption = 'Filetype specific options'
        TabOrder = 1
        object cbCheckGID: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 640
          Height = 17
          Align = alTop
          Caption = 
            'check winhelp sources (*.hlp files) if keyword is valid (help re' +
            'quest may be a bit slower)'
          TabOrder = 0
        end
      end
      inline fccFileProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 650
        Height = 22
        Align = alTop
        AutoSize = True
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 650
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Dynamic Web based providers (RSS)'
      ImageIndex = 4
      inline FrameConfigRSSProviders: TFrameConfigProviders
        Left = 0
        Top = 28
        Width = 656
        Height = 535
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 28
        ExplicitWidth = 656
        ExplicitHeight = 535
        inherited pnlOHSItem: TPanel
          Top = 441
          Width = 656
          ExplicitTop = 441
          ExplicitWidth = 656
          inherited BtnBrowseForFile: TSpeedButton
            Left = 628
            ExplicitLeft = 626
          end
          inherited edName: TEdit
            Width = 560
            ExplicitWidth = 560
          end
          inherited edDesc: TEdit
            Width = 560
            ExplicitWidth = 560
          end
          inherited edURL: TEdit
            Width = 533
            ExplicitWidth = 533
          end
        end
        inherited ListView1: TListView
          Width = 650
          Height = 435
          ExplicitWidth = 650
          ExplicitHeight = 435
        end
        inherited OpenDialog1: TOpenDialog
          Left = 600
          Top = 24
        end
      end
      inline fccRSSProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 650
        Height = 22
        Align = alTop
        AutoSize = True
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 650
      end
    end
    object TabSheet6: TTabSheet
      Caption = '3rd party providers'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 656
        Height = 563
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object FlowPanel1: TFlowPanel
          Left = 0
          Top = 0
          Width = 656
          Height = 41
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitLeft = 28
          ExplicitTop = 3
          ExplicitWidth = 185
        end
      end
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 667
    Top = 3
    Width = 72
    Height = 585
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 66
      Height = 26
      Align = alTop
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 3
      Top = 35
      Width = 66
      Height = 26
      Align = alTop
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
