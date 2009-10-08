object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'IMAPMailChecker - Settings'
  ClientHeight = 325
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 291
    Width = 520
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 434
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 361
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
      ExplicitTop = 0
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 442
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 356
    end
  end
  object gb_Accounts: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 520
    Height = 234
    Align = alClient
    Caption = 'IMAP-Accounts'
    TabOrder = 1
    ExplicitWidth = 434
    ExplicitHeight = 222
    object EffectPNGToolBar1: TEffectPNGToolBar
      Left = 2
      Top = 15
      Width = 516
      Height = 27
      ButtonHeight = 26
      ButtonWidth = 26
      Caption = 'EffectPNGToolBar1'
      Indent = 5
      TabOrder = 0
      ImageEffectDefault.GammaValue = 1.000000000000000000
      ImageEffectDisabled.Effect = peGrayScale
      ImageEffectDisabled.GammaValue = 1.000000000000000000
      object btn_Add: TEffectPNGToolButton
        Left = 5
        Top = 0
        Caption = 'btn_Add'
        OnClick = btn_AddClick
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000016000000160806000000C4B46C
          3B0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000004BC4944415478DA
          B5957D4C55651CC7BFE7F59E8B825080F802174CC01295E865D6FA23BB5CE4CD
          3FC0ADD55A8D52DAC815E02CCA692ECC2CC0DA5CABB036072ACD2D9572A0112F
          C6CBC5522EB9426B595868CE3403F35EEEBDE7ADDF790E528EE83FCFB3B373EF
          F33BCFE7F93DBF97EFE14CD3C4ADB8380BCC3D2B013A0768326072B645D79772
          BAB97F414C529A244A3035CEB6993C6000E72E5DBC14528DB510E5569AB46DAA
          48EBE867FB2FD3800D5D1675E3DB578A37A695BA4B11E99809C3306C4F380EAA
          AEE2A0F730AA766D1ABB160A2F81208EFC3FD810E826632870774A6CCA89E36F
          F40B3151D13871E16B88BC48501EAA19466A4C1AE294383CB2B1185DC7DB4AE0
          9CD9008318063F1D98B78F1A0C2E4D4F5834E07DBD4F3CF5C7377037B8A13814
          C8A28C31750C3B3C3BB0FE9EF528DCFA145ABA0E3D0125AA893965793D05AC92
          AB216305C54F421069F31212EB06DFF209A7C7BEC3CAC65C06B6623DAA8EA22E
          BB0EE559E5E4F16A747535BF0367741B745EA078F78393AE9AEDE726C06BC5BB
          049D3F9C1ABF30C52139610455B86E4FE1F6BED008DF9501E4EF2D6060511499
          C735EE1A546455A0ECBD97D077F298C92B91305403674786AF8C87B512B3F37C
          2B03F325FC4055C1CB59952B2B111D110DDDD0217002644942CF480F721A7326
          C196C73B3D3B519659068D60614D85C0F3086921ECED3888AA77B75CFEAB6D38
          9E81E796CF0BF46DEE7326C7BAD0F27D0B4061E668F0942CDF451FB6F76E872C
          CB947C017EDD8F926525284A2D82C1E9E008AAD37870CE03508C99B8BF2CCF38
          F5D1170203BB2A5D819ECDBDCE9041C17D330DA03C7002A1450E12791DE18860
          FF7982584F8D866E41C9CE0B3C026600070A3E81273E0FCB9F5F65F8EA5B6D70
          524552A07B538F53E7342CAE5BCC160B82C0167192BD98DD049EDC94A779D19E
          1F37C7D194BB0F2B62B3197870D7D1A9E08CB733D8CB93608258B07F832DDB0D
          6FADB9A019C49EDCC6A96057852BD06D8582764EDF96CE1683FA8441640151CE
          A8C95058B0901982351858B443D1BCEA1056C61710B8904271C406CF2F9F3FDE
          B7C5ABCC9E158FC6930DEC9856FAAC2A18FA7D08F5BE7A166B2B7941CA43F1A2
          6278523C5069589B69A68A7C571E62A5044A5EBE31F8E1E736587A5A3AFDDAEA
          EA3BD753B9299272934AF5FEDA0BCF1E0F1499CA4D9A28B71C2AB7656537CB19
          E9D0BECE66ACABDDF0E7E8D1B3B7D90DB24658EE80E3C092C465736628515C28
          1844724C223E287D1F03974FA0609A06797177357A4F7921538368E130867E3C
          333A76FDFA7366D76F1F4F741EB5B4A64750F03CB47314C6B1606E42E2AB8335
          3EFE0CB574EE9EBC9BC0B5EE5AD6D20F6F2CC2979D9FD6C339AB9B4A5985A874
          53562F991D23F807CCE4D2D255CA5A30949196903EE8DDEA2511F2C1BD3B9BBC
          929956F8553F6A736AB1E1DE0D28AC7E122DC79A1F8312B99F899849D9D64D4C
          0533236972C87FDF1D710BFBFB49369D8A031DC3EDD4B6024B54D85091393B13
          C991C9F06C7E14ED7DADCF906CEE862ED8B2FB9FE01B42AF6B8A62723F543FBE
          2DA9D4BD0633E4083A8C751A9369B26668F8ECAB2358B7B33270D5EF5F4A59FD
          C9AA220427C09DD381AD97B4F04374F8260AC93C495460EA139F1FD88B7F3E3F
          3C766D3C5849D086C9CA3038FB2BD572C106DF8AEB6F9E3953F34AFA91850000
          000049454E44AE426082}
      end
      object btn_Delete: TEffectPNGToolButton
        Left = 31
        Top = 0
        Caption = 'btn_Delete'
        Enabled = False
        OnClick = btn_DeleteClick
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000016000000160806000000C4B46C
          3B0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000002404944415478DA
          ED944B6853411486FF99B9B7E656A869342E546A5756D020522BE922820FEA83
          C61674234276822516AAA02E8CA51B5DBA70A575531F142C4A1B050974538C28
          42C5B50FF081B642155B303689B933CE2337B955B35114840E1CCE9C3BC33767
          CE99FF122104FEC6208BE07F032647EC20F2A55B8D8150CCB1EA19175C80CB55
          4EA0BD509E96BD3441CB6B54214065FCADE89299D9D917B0EBBBC4F8EBE7069C
          2037766CD879F84C770AE1A561B89C9B6315C81B7A2E7EF10DA094E24B7E1E83
          6343B89A19991413D36D1ADC7834F4397D221DDCB62EF647D77FF7F10362C9AE
          D2AB9B8F6D0D5E7BBC793EDBFF20B0A4CE46F27612841010660CE6B626A6C628
          A1662EBFA96C0BA280535B4E6263C326448FC5F9934BF7980637F5357DBD7F36
          EB14E58696732D008331AB6CCC801965209684316AA0D22BCBF11CD2FBC7D0B1
          721FA2BD9DFCE960C6074E651D979410B9103110C60CC022955865EF01FDE0BC
          C8E3FAEE6BD8BE629704C77F134C6B80F7D4027BA538EF2B8532BB9AA5AAA79E
          5B55B03A302772188D8FA223BC77612954F3243860DB0C89E144A531CA7BF5D5
          50AF7972AEB2D799CB58356F203A80CDCB5AD12E339EF49AB7BC273C97399D69
          686B6E852B5CFD2A7C8FD52809A4E633533BE47D3033F709ED3D9DEECBE14796
          27903B07B61E8CA7BAFB1172825581FC24928A6017C4443EBF42A988CBE9215C
          1CB9F2AC3431B5DE93F46AB9727755704DC4B11D70A9682D574FC63F4A59F8E5
          ADEEC2E0165DBC9D7E3F2558DD2131FEE6E17FFA775B04ABF11D38311EE41234
          66500000000049454E44AE426082}
      end
    end
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 5
      Top = 45
      Width = 510
      Height = 184
      Align = alClient
      EditDelay = 0
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Indent = 0
      TabOrder = 1
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect]
      OnCreateEditor = VSTCreateEditor
      OnEditing = VSTEditing
      OnFocusChanging = VSTFocusChanging
      OnFreeNode = VSTFreeNode
      OnGetText = VSTGetText
      OnGetNodeDataSize = VSTGetNodeDataSize
      OnNewText = VSTNewText
      ExplicitLeft = 3
      ExplicitTop = 47
      ExplicitHeight = 171
      Columns = <
        item
          Position = 0
          Width = 70
          WideText = 'Host'
        end
        item
          Position = 1
          WideText = 'Port'
        end
        item
          Position = 2
          Width = 70
          WideText = 'Username'
        end
        item
          Position = 3
          Width = 70
          WideText = 'Password'
        end
        item
          Position = 4
          Width = 100
          WideText = 'Mailboxes'
        end
        item
          Position = 5
          Width = 90
          WideText = 'Interval (min)'
        end>
    end
  end
  object gb_Misc: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 243
    Width = 520
    Height = 42
    Align = alBottom
    Caption = 'Misc'
    TabOrder = 2
    object Panel2: TPanel
      Left = 2
      Top = 14
      Width = 516
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 15
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 40
        Height = 20
        Align = alLeft
        Caption = 'Mailtool:'
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object ed_MailTool: TJvFilenameEdit
        AlignWithMargins = True
        Left = 49
        Top = 3
        Width = 464
        Height = 21
        Align = alClient
        Filter = 'Programs (*.exe)|*.exe'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        TabOrder = 0
        ExplicitLeft = 200
        ExplicitTop = 0
        ExplicitWidth = 121
      end
    end
  end
  object SettingsLinkForm: TSettingsLinkForm
    DefaultRootSetting = '/GUI/form_Config'
    SaveProperties.Strings = (
      'Height'
      'Left'
      'Top'
      'Width')
    Settings = Data.SettingsFile
    Left = 480
  end
  object SettingsLinkVST: TSettingsLinkVST
    Settings = Data.SettingsFile
    DefaultRootSetting = '/GUI/form_Config/gb_Accounts/VST'
    Tree = VST
    SaveColumnAllowClick = False
    SaveColumnDraggable = False
    SaveColumnEnabled = False
    SaveColumnParentColor = False
    SaveColumnResizable = False
    SaveColumnShowDropMark = False
    SaveColumnVisible = False
    SaveColumnAutoSpring = False
    SaveColumnFixed = False
    Left = 256
    Top = 168
  end
  object IMAP: TIdIMAP4
    MaxLineAction = maException
    Left = 368
    Top = 160
  end
end
