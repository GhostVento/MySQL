object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 354
  ClientWidth = 738
  Color = clCream
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 221
    Width = 738
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 408
    ExplicitTop = 0
    ExplicitWidth = 309
  end
  object Memo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 227
    Width = 732
    Height = 124
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = 312
  end
  object gbTables: TGroupBox
    AlignWithMargins = True
    Left = 275
    Top = 3
    Width = 130
    Height = 215
    Align = alLeft
    Caption = 'Tables'
    TabOrder = 1
    ExplicitLeft = 139
    ExplicitHeight = 230
    object btn_CreateTable: TButton
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Create Table'
      TabOrder = 0
      OnClick = btn_CreateTableClick
      ExplicitLeft = 8
      ExplicitTop = 39
      ExplicitWidth = 113
    end
    object btn_DestoyTable: TButton
      AlignWithMargins = True
      Left = 5
      Top = 51
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Destoy Table'
      TabOrder = 1
      OnClick = btn_DestoyTableClick
      ExplicitLeft = 32
      ExplicitTop = 39
      ExplicitWidth = 113
    end
    object btn_RenameTable: TButton
      AlignWithMargins = True
      Left = 5
      Top = 82
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Rename Table'
      TabOrder = 2
      OnClick = btn_RenameTableClick
      ExplicitLeft = 3
      ExplicitTop = 131
      ExplicitWidth = 135
    end
    object btn_TableExists: TButton
      AlignWithMargins = True
      Left = 5
      Top = 113
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Table Exists'
      TabOrder = 3
      OnClick = btn_TableExistsClick
      ExplicitLeft = 16
      ExplicitTop = 167
      ExplicitWidth = 113
    end
    object btn_GetTablesInfo: TButton
      AlignWithMargins = True
      Left = 5
      Top = 144
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Tables Info'
      TabOrder = 4
      OnClick = btn_GetTablesInfoClick
      ExplicitLeft = 3
      ExplicitTop = 191
      ExplicitWidth = 113
    end
  end
  object gbConnection: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 130
    Height = 215
    Align = alLeft
    Caption = 'Connection'
    TabOrder = 2
    ExplicitHeight = 230
    object Label1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 51
      Width = 120
      Height = 15
      Align = alTop
      Caption = 'Status:'
      ExplicitLeft = 13
      ExplicitTop = 92
      ExplicitWidth = 175
    end
    object lbl_Status: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 72
      Width = 120
      Height = 15
      Align = alTop
      Caption = 'Disconnected'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 7
      ExplicitTop = 124
      ExplicitWidth = 175
    end
    object btn_Connect: TButton
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Connect'
      TabOrder = 0
      OnClick = btn_ConnectClick
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 113
    end
  end
  object gbColumns: TGroupBox
    AlignWithMargins = True
    Left = 411
    Top = 3
    Width = 130
    Height = 215
    Align = alLeft
    Caption = 'Columns'
    TabOrder = 3
    ExplicitLeft = 275
    ExplicitHeight = 230
    object btn_AddColumn: TButton
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Add Column'
      TabOrder = 0
      OnClick = btn_AddColumnClick
      ExplicitLeft = 7
    end
    object btn_DestroyColumn: TButton
      AlignWithMargins = True
      Left = 5
      Top = 51
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Destroy Column'
      TabOrder = 1
      OnClick = btn_DestroyColumnClick
      ExplicitLeft = 72
      ExplicitTop = 70
      ExplicitWidth = 113
    end
    object btn_RenameColumn: TButton
      AlignWithMargins = True
      Left = 5
      Top = 82
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Rename Column'
      TabOrder = 2
      OnClick = btn_RenameColumnClick
      ExplicitLeft = 32
      ExplicitTop = 111
      ExplicitWidth = 113
    end
    object btn_ColumnExists: TButton
      AlignWithMargins = True
      Left = 5
      Top = 113
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Column Exists'
      TabOrder = 3
      OnClick = btn_ColumnExistsClick
      ExplicitLeft = 40
      ExplicitTop = 150
      ExplicitWidth = 113
    end
    object btn_GetColumnsInfo: TButton
      AlignWithMargins = True
      Left = 5
      Top = 144
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Columns Info'
      TabOrder = 4
      OnClick = btn_GetColumnsInfoClick
      ExplicitLeft = 24
      ExplicitTop = 190
      ExplicitWidth = 113
    end
    object btn_GetColumnType: TButton
      AlignWithMargins = True
      Left = 5
      Top = 175
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Column Type'
      TabOrder = 5
      OnClick = btn_GetColumnTypeClick
      ExplicitLeft = 40
      ExplicitTop = 206
      ExplicitWidth = 113
    end
  end
  object gbRows: TGroupBox
    AlignWithMargins = True
    Left = 547
    Top = 3
    Width = 130
    Height = 215
    Align = alLeft
    Caption = 'Rows'
    TabOrder = 4
    ExplicitLeft = 411
    ExplicitTop = 0
    object btn_AddRow: TButton
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Add Row'
      TabOrder = 0
      OnClick = btn_AddRowClick
      ExplicitLeft = 10
      ExplicitTop = 28
    end
    object btn_ChangeIDName: TButton
      AlignWithMargins = True
      Left = 5
      Top = 51
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Change ID Name'
      TabOrder = 1
      OnClick = btn_ChangeIDNameClick
      ExplicitLeft = 3
      ExplicitTop = 108
    end
    object btn_ChangeLastIdName: TButton
      AlignWithMargins = True
      Left = 5
      Top = 82
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Change Last Id Name'
      TabOrder = 2
      OnClick = btn_ChangeLastIdNameClick
      ExplicitLeft = 7
      ExplicitTop = 113
    end
    object btn_GetValueByID: TButton
      AlignWithMargins = True
      Left = 5
      Top = 113
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Value By ID'
      TabOrder = 3
      OnClick = btn_GetValueByIDClick
      ExplicitLeft = 21
      ExplicitTop = 164
    end
    object btn_GetLastIDValue: TButton
      AlignWithMargins = True
      Left = 5
      Top = 144
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Last ID Value'
      TabOrder = 4
      OnClick = btn_GetLastIDValueClick
      ExplicitLeft = 7
      ExplicitTop = 175
    end
  end
  object gbDatabase: TGroupBox
    AlignWithMargins = True
    Left = 139
    Top = 3
    Width = 130
    Height = 215
    Align = alLeft
    Caption = 'Database'
    TabOrder = 5
    ExplicitLeft = 123
    ExplicitTop = 6
    object btn_CreateDatabase: TButton
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Create Database'
      TabOrder = 0
      OnClick = btn_CreateDatabaseClick
      ExplicitLeft = 10
      ExplicitTop = 28
    end
    object btn_DestroyDatabase: TButton
      AlignWithMargins = True
      Left = 5
      Top = 51
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Destroy Database'
      TabOrder = 1
      OnClick = btn_DestroyDatabaseClick
      ExplicitLeft = 7
    end
    object btn_GetDatabaseInfo: TButton
      AlignWithMargins = True
      Left = 5
      Top = 82
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Get Databases Info'
      TabOrder = 2
      OnClick = btn_GetDatabaseInfoClick
      ExplicitLeft = 3
    end
    object btn_ChangeDatabase: TButton
      AlignWithMargins = True
      Left = 5
      Top = 113
      Width = 120
      Height = 25
      Align = alTop
      Caption = 'Change Database'
      TabOrder = 3
      OnClick = btn_ChangeDatabaseClick
      ExplicitLeft = 0
      ExplicitTop = 144
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 688
    Top = 8
  end
end
