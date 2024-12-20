object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MySQL'
  ClientHeight = 438
  ClientWidth = 800
  Color = 11521228
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 136
    Top = 0
    Height = 416
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 544
    Top = 3
    Width = 253
    Height = 410
    Align = alRight
    Caption = 'Logs'
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 2
      Top = 17
      Height = 391
      ExplicitLeft = 64
      ExplicitTop = 216
      ExplicitHeight = 100
    end
    object Memo: TMemo
      AlignWithMargins = True
      Left = 8
      Top = 20
      Width = 240
      Height = 385
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitWidth = 316
    end
  end
  object gbConnection: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 130
    Height = 410
    Align = alLeft
    Caption = 'Connection'
    TabOrder = 1
    ExplicitHeight = 401
    object edt_Driver: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 30
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      TabOrder = 0
      Text = 'MySQL'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Driver'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_Server: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 69
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      TabOrder = 1
      Text = 'localhost'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Server'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_Port: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 108
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      NumbersOnly = True
      TabOrder = 2
      Text = '3306'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Port'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_UserName: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 147
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      TabOrder = 3
      Text = 'root'
      BoundLabel.Active = True
      BoundLabel.Caption = 'UserName'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_Password: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 186
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      PasswordChar = '*'
      TabOrder = 4
      Text = 'root'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Password'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_Database: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 225
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      TabOrder = 5
      Text = 'information_schema'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Database'
      BoundLabel.Layout = sclTopLeft
    end
    object edt_Charset: TsEdit
      AlignWithMargins = True
      Left = 5
      Top = 264
      Width = 120
      Height = 23
      Margins.Top = 13
      Align = alTop
      TabOrder = 6
      Text = 'utf8mb4'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Charset'
      BoundLabel.Layout = sclTopLeft
    end
    object btn_Connect: TButton
      AlignWithMargins = True
      Left = 5
      Top = 293
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Connect'
      TabOrder = 7
      OnClick = btn_ConnectClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 416
    Width = 800
    Height = 22
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 407
    ExplicitWidth = 794
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 16
      Align = alLeft
      Caption = 'Status: '
      ExplicitHeight = 15
    end
    object lbl_Status: TLabel
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 72
      Height = 16
      Align = alLeft
      Caption = 'Disconnected'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitHeight = 15
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 142
    Top = 3
    Width = 396
    Height = 410
    Align = alClient
    Caption = 'Grid'
    TabOrder = 3
    ExplicitLeft = 496
    ExplicitWidth = 301
    object DBGrid1: TDBGrid
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 386
      Height = 385
      Align = alClient
      DataSource = DataSource1
      ReadOnly = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 688
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 475
    Top = 51
    object N1: TMenuItem
      Caption = 'Database'
      object D1_GetDBInfo: TMenuItem
        Caption = 'Get Database Info'
        OnClick = D1_GetDBInfoClick
      end
      object C1_CreateDB: TMenuItem
        Caption = 'Create Database'
        OnClick = C1_CreateDBClick
      end
      object D2_DestroyDB: TMenuItem
        Caption = 'Destroy Database'
        OnClick = D2_DestroyDBClick
      end
      object C2_ChangeDB: TMenuItem
        Caption = 'Change Database'
        OnClick = C2_ChangeDBClick
      end
      object D3_DBExists: TMenuItem
        Caption = 'Databse Exists'
        OnClick = D3_DBExistsClick
      end
    end
    object T1: TMenuItem
      Caption = 'Tables'
      object G1_GetTablesInfo: TMenuItem
        Caption = 'Get Tables Info'
        OnClick = G1_GetTablesInfoClick
      end
      object C1_CreateTable: TMenuItem
        Caption = 'Create Table'
        OnClick = C1_CreateTableClick
      end
      object D1_DestroyTable: TMenuItem
        Caption = 'Destroy Table'
        OnClick = D1_DestroyTableClick
      end
      object R1_RenameTable: TMenuItem
        Caption = 'Rename Table'
        OnClick = R1_RenameTableClick
      end
      object T2_TableExists: TMenuItem
        Caption = 'Table Exists'
        OnClick = T2_TableExistsClick
      end
    end
    object C1: TMenuItem
      Caption = 'Columns'
      object G1_GetColsInfo: TMenuItem
        Caption = 'Get Columns Info'
        OnClick = G1_GetColsInfoClick
      end
      object G1_GetColType: TMenuItem
        Caption = 'Get Column Type'
        OnClick = G1_GetColTypeClick
      end
      object A1_AddCol: TMenuItem
        Caption = 'Add Column'
        OnClick = A1_AddColClick
      end
      object D1_DestroyCol: TMenuItem
        Caption = 'Destroy Column'
        OnClick = D1_DestroyColClick
      end
      object R2_RenameCol: TMenuItem
        Caption = 'Rename Column'
        OnClick = R2_RenameColClick
      end
      object C2_ColExists: TMenuItem
        Caption = 'Column Exists'
        OnClick = C2_ColExistsClick
      end
    end
    object R1: TMenuItem
      Caption = 'Rows'
      object A1_AddRow: TMenuItem
        Caption = 'Add Row'
        OnClick = A1_AddRowClick
      end
      object G1_GetInfoByID: TMenuItem
        Caption = 'Get Value by ID+Col'
        OnClick = G1_GetInfoByIDClick
      end
      object G2_GetLastIDInfo: TMenuItem
        Caption = 'Get Value by Last ID+Col'
        OnClick = G2_GetLastIDInfoClick
      end
      object C2_ChangeNameByID: TMenuItem
        Caption = 'Change Value in '#39'name'#39' by ID'
        OnClick = C2_ChangeNameByIDClick
      end
      object C3_ChangeLastIDName: TMenuItem
        Caption = 'Change Value in '#39'name'#39' by Last ID'
        OnClick = C3_ChangeLastIDNameClick
      end
      object C2_ChangeValueByIdAndCol: TMenuItem
        Caption = 'Change Value by ID+Col'
        OnClick = C2_ChangeValueByIdAndColClick
      end
      object G1_GetFullRowInfoByID: TMenuItem
        Caption = 'Get Full Row Info by ID'
        OnClick = G1_GetFullRowInfoByIDClick
      end
      object S1_SearchRowsByName: TMenuItem
        Caption = 'Search Rows by Name'
        OnClick = S1_SearchRowsByNameClick
      end
      object S1_SearchByColValue: TMenuItem
        Caption = 'Search Rows by Col+Value'
        OnClick = S1_SearchByColValueClick
      end
    end
    object N2: TMenuItem
      Caption = 'Grid'
      object L1_TableToGrid: TMenuItem
        Caption = 'Load Table to Grid'
        OnClick = L1_TableToGridClick
      end
    end
  end
  object DataSource1: TDataSource
    Left = 446
    Top = 139
  end
end
