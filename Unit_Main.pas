unit Unit_Main;

interface

uses
  {Winapi}
  Winapi.Windows, Winapi.Messages,
  {System}
  System.SysUtils, System.Variants, System.Classes,
  {Vcl}
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Timer: TTimer;
    Memo: TMemo;
    gbTables: TGroupBox;
    btn_CreateTable: TButton;
    btn_DestoyTable: TButton;
    gbConnection: TGroupBox;
    btn_Connect: TButton;
    Label1: TLabel;
    lbl_Status: TLabel;
    btn_RenameTable: TButton;
    btn_TableExists: TButton;
    btn_GetTablesInfo: TButton;
    gbColumns: TGroupBox;
    btn_AddColumn: TButton;
    btn_DestroyColumn: TButton;
    btn_RenameColumn: TButton;
    btn_ColumnExists: TButton;
    btn_GetColumnsInfo: TButton;
    btn_GetColumnType: TButton;
    Splitter1: TSplitter;
    gbRows: TGroupBox;
    btn_AddRow: TButton;
    btn_ChangeIDName: TButton;
    btn_ChangeLastIdName: TButton;
    btn_GetValueByID: TButton;
    btn_GetLastIDValue: TButton;
    gbDatabase: TGroupBox;
    btn_CreateDatabase: TButton;
    btn_DestroyDatabase: TButton;
    btn_GetDatabaseInfo: TButton;
    btn_ChangeDatabase: TButton;
    procedure btn_ConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_CreateTableClick(Sender: TObject);
    procedure btn_AddColumnClick(Sender: TObject);
    procedure btn_DestoyTableClick(Sender: TObject);
    procedure btn_RenameTableClick(Sender: TObject);
    procedure btn_TableExistsClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btn_DestroyColumnClick(Sender: TObject);
    procedure btn_RenameColumnClick(Sender: TObject);
    procedure btn_ColumnExistsClick(Sender: TObject);
    procedure btn_GetColumnTypeClick(Sender: TObject);
    procedure btn_GetColumnsInfoClick(Sender: TObject);
    procedure btn_GetTablesInfoClick(Sender: TObject);
    procedure btn_AddRowClick(Sender: TObject);
    procedure btn_ChangeIDNameClick(Sender: TObject);
    procedure btn_ChangeLastIdNameClick(Sender: TObject);
    procedure btn_GetValueByIDClick(Sender: TObject);
    procedure btn_GetLastIDValueClick(Sender: TObject);
    procedure btn_CreateDatabaseClick(Sender: TObject);
    procedure btn_DestroyDatabaseClick(Sender: TObject);
    procedure btn_GetDatabaseInfoClick(Sender: TObject);
    procedure btn_ChangeDatabaseClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit_SQL;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQL := TSQL.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQL.Free;
end;

{ Connection }

procedure TForm1.btn_ConnectClick(Sender: TObject);
begin
  if SQL.FDConnection.Connected then
  begin
    if SQL.Disconnect then
    begin
      with lbl_Status do
      begin
        Caption := 'Disconnected';
        Font.Color := clRed;
      end;
      btn_Connect.Caption := 'Connect';
    end;
  end
  else
  begin
    if SQL.Connect then
    begin
      with lbl_Status do
      begin
        Caption := 'Connected';
        Font.Color := clGreen;
      end;
      btn_Connect.Caption := 'Disconnect';
    end
  end;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  if SQL.FDConnection.Connected then
  begin
    with lbl_Status do
    begin
      Caption := 'Connected';
      Font.Color := clGreen;
    end;
    btn_Connect.Caption := 'Disconnect';
  end
  else
  begin
    with lbl_Status do
    begin
      Caption := 'Disconnected';
      Font.Color := clRed;
    end;
    btn_Connect.Caption := 'Connect';
  end;
end;

{ Database }

procedure TForm1.btn_CreateDatabaseClick(Sender: TObject);
var
  DBName, Charset, Collation: String;
begin
  DBName := InputBox('DB Name', 'DB Name', 'test_db');
  Charset := InputBox('Charset', 'Charset', 'utf8mb4');
  Collation := InputBox('Collation', 'Collation', 'utf8mb4_unicode_ci');
  if (Trim(DBName) <> '') and (Trim(Charset) <> '') and (Trim(Collation) <> '')
  then
    if SQL.CreateDatabase(DBName, Charset, Collation) then
      ShowMessage
        (Format('DB "%s" created with charset "%s" and collation "%s".',
        [DBName, Charset, Collation]));
end;

procedure TForm1.btn_DestroyDatabaseClick(Sender: TObject);
var
  DBName: String;
begin
  DBName := InputBox('DB Name', 'DB Name', 'test_db');
  if Trim(DBName) <> '' then

    if SQL.DestroyDatabase(DBName) then
      ShowMessage('DB ' + DBName + ' destroyed');
end;

procedure TForm1.btn_GetDatabaseInfoClick(Sender: TObject);
var
  Databases: TStringList;
  I: Integer;
begin
  Databases := SQL.GetDatabaseList;
  try
    // Выводим список баз данных в TMemo
    for I := 0 to Databases.Count - 1 do
      Memo.Lines.Add(Databases[I]);
  finally
    Databases.Free;
  end;
end;

procedure TForm1.btn_ChangeDatabaseClick(Sender: TObject);
var
  OldDBName, NewDBName, Charset, Collation: String;
begin
  OldDBName := InputBox('DB Name', 'DB Name', 'old_db');
  NewDBName := InputBox('New DB Name', 'New DB Name', 'new_db');
  Charset := InputBox('Charset', 'Charset', 'utf8mb4');
  Collation := InputBox('Collation', 'Collation', 'utf8mb4_unicode_ci');
  if (Trim(OldDBName) <> '') and (Trim(NewDBName) <> '') and
    (Trim(Charset) <> '') and (Trim(Collation) <> '') then
  if SQL.ChangeDatabase(OldDBName, NewDBName, Charset, Collation) then
    ShowMessage(Format('DB "%s" renamed to "%s".', [OldDBName, NewDBName]))
  else
    ShowMessage('Error');
end;

{ Table }

procedure TForm1.btn_CreateTableClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  if Trim(Table) <> '' then
    if SQL.CreateTable(Table) then
      Memo.Lines.Add('Table ' + Table + ' created')
    else
      Memo.Lines.Add('Table ' + Table + ' not created')
end;

procedure TForm1.btn_DestoyTableClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  if Trim(Table) <> '' then
    if SQL.DestroyTable(Table) then
      Memo.Lines.Add('Table ' + Table + ' destroyed')
    else
      Memo.Lines.Add('Table ' + Table + ' not destoyed');
end;

procedure TForm1.btn_RenameTableClick(Sender: TObject);
var
  TableOld: String;
  TableNew: String;
begin
  TableOld := InputBox('Old Table Name', 'Old Table Name', '');
  TableNew := InputBox('New Table Name', 'New Table Name', '');
  if (Trim(TableOld) <> '') and (Trim(TableNew) <> '') then
    if SQL.RenameTable(TableOld, TableNew) then
      Memo.Lines.Add('Table ' + TableOld + ' renamed to ' + TableNew +
        ' successfully')
    else
      Memo.Lines.Add('Table  ' + TableOld + ' not renamed')
end;

procedure TForm1.btn_TableExistsClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  if Trim(Table) <> '' then
    if SQL.TableExists(Table) then
      Memo.Lines.Add('Table ' + Table + ' exists')
    else
      Memo.Lines.Add('Table ' + Table + ' not exists')
end;

{ Column }

procedure TForm1.btn_AddColumnClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.AddColumn(Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' has been added to Table ' + Table + ' successfully')
    else
      Memo.Lines.Add('Column ' + Column + ' was not added to Table ' + Table)
end;

procedure TForm1.btn_DestroyColumnClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.DestroyColumn(Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' has been deleted from Table ' + Table + ' successfully')
    else
      Memo.Lines.Add('Column ' + Column + ' was not deleted from Table ' + Table)
end;

procedure TForm1.btn_RenameColumnClick(Sender: TObject);
var
  Table: String;
  ColumnOld: String;
  ColumnNew: String;
  ColumnType: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  ColumnOld := InputBox('Column Name', 'Column Name', '');
  ColumnNew := InputBox('New Column Name', 'New Column Name', '');
  ColumnType := InputBox('Column Data Type', 'Column Data Type',
    SQL.GetColumnType(Table, ColumnOld));
  if (Trim(Table) <> '') and (Trim(ColumnOld) <> '') and (Trim(ColumnNew) <> '')
    and (Trim(ColumnType) <> '') then
    if SQL.RenameColumn(Table, ColumnOld, ColumnNew, ColumnType) then
      Memo.Lines.Add('Column ' + ColumnOld + ' renamed to ' + ColumnNew +
        ' successfully')
    else
      Memo.Lines.Add('Column ' + ColumnOld + ' was not renamed');
end;

procedure TForm1.btn_AddRowClick(Sender: TObject);
var
  Table: String;
  Name, Position: String;
  Salary: Double;
  HireDate: TDate;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Name := InputBox('Name', 'Name', 'John Doe');
  Position := InputBox('Position', 'Position', 'Manager');
  Salary := StrToFloat(InputBox('Salary', 'Salary', '50000,00'));
  HireDate := StrToDate(InputBox('HireDate', 'HireDate', DateToStr(Now)));
  if (Trim(Table) <> '') and (Trim(Name) <> '') and (Trim(Position) <> '') and
    (Trim(FloatToStr(Salary)) <> '') and (Trim(DateToStr(HireDate)) <> '') then
    if SQL.InsertRow(Table, Name, Position, Salary, HireDate) then
      ShowMessage('New Row added successfully.')
    else
      ShowMessage('Error.');
end;

procedure TForm1.btn_ColumnExistsClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.ColumnExists(Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' exists in Table ' + Table)
    else
      Memo.Lines.Add('Column ' + Column + ' not exists in Table ' + Table)
end;

procedure TForm1.btn_GetColumnTypeClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    Memo.Lines.Add('Data Type ' + Column + ' ' +
      SQL.GetColumnType(Table, Column));
end;

procedure TForm1.btn_GetTablesInfoClick(Sender: TObject);
begin
  Memo.Lines.Assign(SQL.GetTablesInfo);
end;

procedure TForm1.btn_GetColumnsInfoClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  if (Trim(Table) <> '') then
  begin
    Memo.Lines.Assign(SQL.GetColumnsInfo(Table));
  end;
end;

procedure TForm1.btn_ChangeIDNameClick(Sender: TObject);
var
  Table: String;
  RowID: Integer;
  NewName: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  NewName := InputBox('New Name', 'New Name', 'Jane Doe');
  if (Trim(Table) <> '') and (Trim(IntToStr(RowID)) <> '') and
    (Trim(NewName) <> '') then
    if SQL.UpdateRowName(Table, RowID, NewName) then
      ShowMessage('Name updated successfully.')
    else
      ShowMessage('Error');
end;

procedure TForm1.btn_ChangeLastIdNameClick(Sender: TObject);
var
  Table: String;
  NewName: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  NewName := InputBox('New Name', 'New Name', 'Jane Doe');
  if (Trim(Table) <> '') and (Trim(NewName) <> '') then
    if SQL.UpdateLastRowName(Table, NewName) then
      ShowMessage('Name updated successfully.')
    else
      ShowMessage('Error.');
end;

procedure TForm1.btn_GetValueByIDClick(Sender: TObject);
var
  Table, Column, Value: String;
  RowID: Integer;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column', 'Column', 'name');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  if (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(IntToStr(RowID)) <> '') then
    Value := SQL.GetRowValueByID(Table, RowID, Column);
  Memo.Lines.Add('Name ID ' + RowID.ToString + ': ' + Value);
end;

procedure TForm1.btn_GetLastIDValueClick(Sender: TObject);
var
  Table, Column, Value: String;
begin
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column', 'Column', 'name');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then

    Value := SQL.GetLastRowValue(Table, Column);
  Memo.Lines.Add('Last ID Name: ' + Value);
end;

end.
