object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 35
    Height = 15
    Caption = 'Status:'
  end
  object lbl_Status: TLabel
    Left = 49
    Top = 48
    Width = 72
    Height = 15
    Caption = 'Disconnected'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object btn_Connect: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = btn_ConnectClick
  end
  object btn_CreateTable: TButton
    Left = 8
    Top = 80
    Width = 113
    Height = 25
    Caption = 'Create Table'
    TabOrder = 1
    OnClick = btn_CreateTableClick
  end
  object btn_AddColumn: TButton
    Left = 8
    Top = 111
    Width = 113
    Height = 25
    Caption = 'Add Column'
    TabOrder = 2
    OnClick = btn_AddColumnClick
  end
  object btn_DestoyTable: TButton
    Left = 127
    Top = 80
    Width = 113
    Height = 25
    Caption = 'Destoy Table'
    TabOrder = 3
    OnClick = btn_DestoyTableClick
  end
  object btn_RenameTable: TButton
    Left = 246
    Top = 80
    Width = 113
    Height = 25
    Caption = 'Rename Table'
    TabOrder = 4
    OnClick = btn_RenameTableClick
  end
  object btn_TableExists: TButton
    Left = 365
    Top = 80
    Width = 113
    Height = 25
    Caption = 'Table Exists'
    TabOrder = 5
    OnClick = btn_TableExistsClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 440
    Top = 8
  end
end
