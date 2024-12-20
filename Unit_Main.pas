unit Unit_Main;

interface

uses
  {Winapi}
  Winapi.Windows, Winapi.Messages,
  {System}
  System.SysUtils, System.Variants, System.Classes, System.Math,
  {Vcl}
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, sGroupBox, sEdit, Vcl.Menus, Vcl.ComCtrls, Data.DB, Vcl.Grids,
  Vcl.DBGrids;

type
  TForm1 = class(TForm)
    Timer: TTimer;
    GroupBox1: TGroupBox;
    Memo: TMemo;
    gbConnection: TGroupBox;
    edt_Driver: TsEdit;
    edt_Server: TsEdit;
    edt_Port: TsEdit;
    edt_UserName: TsEdit;
    edt_Password: TsEdit;
    edt_Database: TsEdit;
    edt_Charset: TsEdit;
    btn_Connect: TButton;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    D1_GetDBInfo: TMenuItem;
    C1_CreateDB: TMenuItem;
    D2_DestroyDB: TMenuItem;
    C2_ChangeDB: TMenuItem;
    D3_DBExists: TMenuItem;
    T1: TMenuItem;
    G1_GetTablesInfo: TMenuItem;
    C1_CreateTable: TMenuItem;
    D1_DestroyTable: TMenuItem;
    R1_RenameTable: TMenuItem;
    T2_TableExists: TMenuItem;
    C1: TMenuItem;
    R1: TMenuItem;
    G1_GetColsInfo: TMenuItem;
    G1_GetColType: TMenuItem;
    A1_AddCol: TMenuItem;
    D1_DestroyCol: TMenuItem;
    R2_RenameCol: TMenuItem;
    C2_ColExists: TMenuItem;
    G1_GetInfoByID: TMenuItem;
    G2_GetLastIDInfo: TMenuItem;
    C2_ChangeNameByID: TMenuItem;
    C3_ChangeLastIDName: TMenuItem;
    A1_AddRow: TMenuItem;
    S1_SearchRowsByName: TMenuItem;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label1: TLabel;
    lbl_Status: TLabel;
    G1_GetFullRowInfoByID: TMenuItem;
    C2_ChangeValueByIdAndCol: TMenuItem;
    S1_SearchByColValue: TMenuItem;
    N2: TMenuItem;
    L1_TableToGrid: TMenuItem;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    procedure btn_ConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure D1_GetDBInfoClick(Sender: TObject);
    procedure C1_CreateDBClick(Sender: TObject);
    procedure D2_DestroyDBClick(Sender: TObject);
    procedure C2_ChangeDBClick(Sender: TObject);
    procedure D3_DBExistsClick(Sender: TObject);
    procedure G1_GetTablesInfoClick(Sender: TObject);
    procedure C1_CreateTableClick(Sender: TObject);
    procedure D1_DestroyTableClick(Sender: TObject);
    procedure R1_RenameTableClick(Sender: TObject);
    procedure T2_TableExistsClick(Sender: TObject);
    procedure G1_GetColsInfoClick(Sender: TObject);
    procedure G1_GetColTypeClick(Sender: TObject);
    procedure A1_AddColClick(Sender: TObject);
    procedure D1_DestroyColClick(Sender: TObject);
    procedure R2_RenameColClick(Sender: TObject);
    procedure C2_ColExistsClick(Sender: TObject);
    procedure G1_GetInfoByIDClick(Sender: TObject);
    procedure G2_GetLastIDInfoClick(Sender: TObject);
    procedure C2_ChangeNameByIDClick(Sender: TObject);
    procedure C3_ChangeLastIDNameClick(Sender: TObject);
    procedure A1_AddRowClick(Sender: TObject);
    procedure S1_SearchRowsByNameClick(Sender: TObject);
    procedure G1_GetFullRowInfoByIDClick(Sender: TObject);
    procedure C2_ChangeValueByIdAndColClick(Sender: TObject);
    procedure S1_SearchByColValueClick(Sender: TObject);
    procedure L1_TableToGridClick(Sender: TObject);

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
  SQL.Driver := edt_Driver.Text;
  SQL.Server := edt_Server.Text;
  SQL.Port := edt_Port.Text;;
  SQL.UserName := edt_UserName.Text;
  SQL.Password := edt_Password.Text;
  SQL.Database := edt_Database.Text;
  SQL.Charset := edt_Charset.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQL.Free;
end;

procedure TForm1.G1_GetColsInfoClick(Sender: TObject);
var
  Database, Table: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(Table) <> '') then
    // Output
    Memo.Lines.Assign(SQL.GetColumnsInfo(Database, Table));
end;

procedure TForm1.G1_GetColTypeClick(Sender: TObject);
var
  Database, Table, Column: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '')
  then
    Memo.Lines.Add('Data Type ' + Column + ' ' + SQL.GetColumnType(Database,
      Table, Column));
end;

procedure TForm1.G1_GetFullRowInfoByIDClick(Sender: TObject);
var
  Database, Table: String;
  RowID: Integer;
  RowValues: TStringList;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  RowID := StrToInt(InputBox('ID', 'ID', ''));

  if (Trim(Database) <> '') and (Trim(Table) <> '') and (RowID > 0) then
  begin
    RowValues := SQL.GetRowValuesByID(Database, Table, RowID);
    if RowValues.Count > 0 then
    begin
      Memo.Lines.Assign(RowValues);
      // Используем Assign, чтобы присвоить строки в Memo
    end
    else
    begin
      Memo.Lines.Add('No data found for the given ID.');
    end;
    RowValues.Free; // Не забываем освободить память
  end
  else
    Memo.Lines.Add('Invalid input.');
end;

procedure TForm1.G1_GetInfoByIDClick(Sender: TObject);
var
  Database, Table, Column, Value: String;
  RowID: Integer;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column', 'Column', 'name');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(IntToStr(RowID)) <> '') then
    Value := SQL.GetRowValueByID(Database, Table, RowID, Column);
  Memo.Lines.Add('Name ID ' + RowID.ToString + ': ' + Value);
end;

procedure TForm1.G1_GetTablesInfoClick(Sender: TObject);
var
  Database: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  // Check for empty input
  if Trim(Database) <> '' then
    // Output
    Memo.Lines.Assign(SQL.GetTablesInfo(Database));
end;

procedure TForm1.G2_GetLastIDInfoClick(Sender: TObject);
var
  Database, Table, Column, Value: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column', 'Column', 'name');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '')
  then
    Value := SQL.GetLastRowValue(Database, Table, Column);
  Memo.Lines.Add('Last ID Name: ' + Value);
end;

procedure AutoSizeDBGridColumns(DBGrid: TDBGrid);
var
  i, MaxWidth: Integer;
  TempStr: String;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
  begin
    MaxWidth := 0; // Для каждой колонки начинаем с нуля
    // Пройдемся по всем записям в DataSet и найдем максимальную длину строки в данной колонке
    DBGrid.DataSource.DataSet.First;
    while not DBGrid.DataSource.DataSet.Eof do
    begin
      TempStr := DBGrid.DataSource.DataSet.Fields[i].AsString;
      MaxWidth := Max(MaxWidth, Length(TempStr)); // Сравниваем и сохраняем максимальную длину
      DBGrid.DataSource.DataSet.Next;
    end;

    // Устанавливаем ширину колонки, добавляем небольшой отступ (например, 5 пикселей)
    DBGrid.Columns[i].Width := MaxWidth * 9 + 10; // 7 - это приблизительная ширина символа в пикселях
  end;
end;

procedure TForm1.L1_TableToGridClick(Sender: TObject);
var
  Database, Table: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(Table) <> '') then
    // Output
    if SQL.LoadTableIntoDBGrid(Database, Table, DataSource1) then
      Memo.Lines.Add('Data loaded successfully!')
    else
      Memo.Lines.Add('Failed to load data.');
  AutoSizeDBGridColumns(DBGrid1);
end;

procedure TForm1.R1_RenameTableClick(Sender: TObject);
var
  Database, TableOld, TableNew: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  TableOld := InputBox('Old Table Name', 'Old Table Name', '');
  TableNew := InputBox('New Table Name', 'New Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(TableOld) <> '') and (Trim(TableNew) <> '')
  then
    // Output
    if SQL.RenameTable(Database, TableOld, TableNew) then
      Memo.Lines.Add(Format('Table "%s" renamed to "%s" successfully',
        [TableOld, TableNew]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.R2_RenameColClick(Sender: TObject);
var
  Database, Table, ColumnOld, ColumnNew, ColumnType: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  ColumnOld := InputBox('Column Name', 'Column Name', '');
  ColumnNew := InputBox('New Column Name', 'New Column Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(ColumnOld) <> '')
  then
    ColumnType := InputBox('Column Data Type', 'Column Data Type',
      SQL.GetColumnType(Database, Table, ColumnOld));
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(ColumnOld) <> '')
    and (Trim(ColumnNew) <> '') and (Trim(ColumnType) <> '') then
    if SQL.RenameColumn(Database, Table, ColumnOld, ColumnNew, ColumnType) then
      Memo.Lines.Add('Column ' + ColumnOld + ' renamed to ' + ColumnNew +
        ' successfully')
    else
      Memo.Lines.Add('Column ' + ColumnOld + ' was not renamed');
end;

procedure TForm1.S1_SearchByColValueClick(Sender: TObject);
var
  Database, Table, Column, Value: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  Value := InputBox('Value', 'Value', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(Value) <> '') then
    Memo.Lines.Assign(SQL.FindRowsByColumn(Database, Table, Column, Value));
end;

procedure TForm1.S1_SearchRowsByNameClick(Sender: TObject);
var
  Database, Table, Name: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Name := InputBox('Find by Name', 'Find by Name', '');

  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Name) <> '') then
    Memo.Lines.Assign(SQL.FindRowsByName(Database, Table, Name));
end;

procedure TForm1.A1_AddRowClick(Sender: TObject);
var
  Database, Table, Name, Position: String;
  Salary: Double;
  HireDate: TDate;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Name := InputBox('Name', 'Name', 'John Doe');
  Position := InputBox('Position', 'Position', 'Manager');
  Salary := StrToFloat(InputBox('Salary', 'Salary', '50000,00'));
  HireDate := StrToDate(InputBox('HireDate', 'HireDate', DateToStr(Now)));
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Name) <> '') and
    (Trim(Position) <> '') and (Trim(FloatToStr(Salary)) <> '') and
    (Trim(DateToStr(HireDate)) <> '') then
    if SQL.InsertRow(Database, Table, Name, Position, Salary, HireDate) then
      Memo.Lines.Add('New Row added successfully.');
end;

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
    SQL.Driver := edt_Driver.Text;
    SQL.Server := edt_Server.Text;
    SQL.Port := edt_Port.Text;;
    SQL.UserName := edt_UserName.Text;
    SQL.Password := edt_Password.Text;
    SQL.Database := edt_Database.Text;
    SQL.Charset := edt_Charset.Text;

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

procedure TForm1.T2_TableExistsClick(Sender: TObject);
var
  Database, Table: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(Table) <> '') then
    // Output
    if SQL.TableExists(Database, Table) then
      Memo.Lines.Add(Format('Table "%s" exists', [Table]))
    else
      Memo.Lines.Add(Format('Table "%s" does not exists', [Table]))
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

procedure TForm1.C1_CreateDBClick(Sender: TObject);
var
  DBName, Charset, Collation: String;
begin
  // Input
  DBName := InputBox('DB Name', 'DB Name', '');
  Charset := InputBox('Charset', 'Charset', 'utf8mb4');
  Collation := InputBox('Collation', 'Collation', 'utf8mb4_unicode_ci');

  // Check for empty input
  if (Trim(DBName) <> '') and (Trim(Charset) <> '') and (Trim(Collation) <> '')
  then
    // Output
    if SQL.CreateDatabase(DBName, Charset, Collation) then
      Memo.Lines.Add
        (Format('DB "%s" created with charset "%s" and collation "%s".',
        [DBName, Charset, Collation]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.C1_CreateTableClick(Sender: TObject);
var
  Database, Table: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(Table) <> '') then
    // Output
    if SQL.CreateTable(Database, Table) then
      Memo.Lines.Add(Format('Table "%s" created in DB "%s"', [Table, Database]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.C2_ChangeDBClick(Sender: TObject);
var
  OldDBName, NewDBName, Charset, Collation: String;
begin
  // Input
  OldDBName := InputBox('DB Name', 'DB Name', '');
  NewDBName := InputBox('New DB Name', 'New DB Name', '');
  Charset := InputBox('Charset', 'Charset', 'utf8mb4');
  Collation := InputBox('Collation', 'Collation', 'utf8mb4_unicode_ci');
  // Check for empty input
  if (Trim(OldDBName) <> '') and (Trim(NewDBName) <> '') and
    (Trim(Charset) <> '') and (Trim(Collation) <> '') then
    // Output
    if SQL.ChangeDatabase(OldDBName, NewDBName, Charset, Collation) then
      Memo.Lines.Add(Format('DB "%s" renamed to "%s".', [OldDBName, NewDBName]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.C2_ChangeNameByIDClick(Sender: TObject);
var
  Database, Table, NewName: String;
  RowID: Integer;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  NewName := InputBox('New Name', 'New Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and
    (Trim(IntToStr(RowID)) <> '') and (Trim(NewName) <> '') then
    if SQL.UpdateRowName(Database, Table, RowID, NewName) then
      Memo.Lines.Add('Name updated successfully.');
end;

procedure TForm1.C2_ChangeValueByIdAndColClick(Sender: TObject);
var
  Database, Table, Column, NewValue: String;
  RowID: Integer;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(IntToStr(RowID)) <> '') then
    NewValue := InputBox('New Value', 'New Value', SQL.GetRowValueByID(Database,
      Table, RowID, Column));
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(IntToStr(RowID)) <> '') and (Trim(NewValue) <> '') then
    if SQL.UpdateRow(Database, Table, Column, RowID, NewValue) then
      Memo.Lines.Add('Value changed successfully.');

end;

procedure TForm1.C2_ColExistsClick(Sender: TObject);
var
  Database, Table, Column: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '')
  then
    if SQL.ColumnExists(Database, Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' exists in Table ' + Table)
    else
      Memo.Lines.Add('Column ' + Column + ' not exists in Table ' + Table)
end;

procedure TForm1.C3_ChangeLastIDNameClick(Sender: TObject);
var
  Database, Table, NewName: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  NewName := InputBox('New Name', 'New Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(NewName) <> '')
  then
    if SQL.UpdateLastRowName(Database, Table, NewName) then
      Memo.Lines.Add('Name updated successfully.');
end;

procedure TForm1.D1_DestroyColClick(Sender: TObject);
var
  Database, Table, Column: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '')
  then
    if SQL.DestroyColumn(Database, Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' has been deleted from Table ' +
        Table + ' successfully')
    else
      Memo.Lines.Add('Column ' + Column +
        ' was not deleted from Table ' + Table)
end;

procedure TForm1.D1_DestroyTableClick(Sender: TObject);
var
  Database, Table: String;
begin
  // Input
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  // Check for empty input
  if (Trim(Database) <> '') and (Trim(Table) <> '') then
    // Output
    if SQL.DestroyTable(Database, Table) then
      Memo.Lines.Add(Format('Table "%s" destroyed in DB "%s"',
        [Table, Database]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.D1_GetDBInfoClick(Sender: TObject);
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

procedure TForm1.D2_DestroyDBClick(Sender: TObject);
var
  DBName: String;
begin
  // Input
  DBName := InputBox('DB Name', 'DB Name', '');
  // Check for empty input
  if Trim(DBName) <> '' then
    // Output
    if SQL.DestroyDatabase(DBName) then
      Memo.Lines.Add(Format('DB "%s" destroyed', [DBName]))
    else
      ShowMessage(SQL.LastError);
end;

procedure TForm1.D3_DBExistsClick(Sender: TObject);
var
  DBName: String;
begin
  // Input
  DBName := InputBox('DB Name', 'DB Name', '');
  // Check for empty input
  if Trim(DBName) <> '' then
    // Output
    if SQL.DatabaseExists(DBName) then
      Memo.Lines.Add(Format('DB "%s" exists', [DBName]))
    else
      Memo.Lines.Add(Format('DB "%s" does not exists', [DBName]));
end;

procedure TForm1.A1_AddColClick(Sender: TObject);
var
  Database, Table, Column: String;
begin
  Database := InputBox('Database', 'Database', '');
  Table := InputBox('Table Name', 'Table Name', '');
  Column := InputBox('Column Name', 'Column Name', '');
  if (Trim(Database) <> '') and (Trim(Table) <> '') and (Trim(Column) <> '')
  then
    if SQL.AddColumn(Database, Table, Column) then
      Memo.Lines.Add('Column ' + Column + ' has been added to Table ' + Table +
        ' successfully')
    else
      Memo.Lines.Add('Column ' + Column + ' was not added to Table ' + Table)
end;

end.
