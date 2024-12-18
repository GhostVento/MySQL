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

{ Table }

procedure TForm1.btn_CreateTableClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(Table) <> '' then
    if SQL.CreateTable(Table) then
      Memo.Lines.Add('Таблица  ' + Table + ' создана')
    else
      Memo.Lines.Add('Таблица  ' + Table + ' не создана')
end;

procedure TForm1.btn_DestoyTableClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(Table) <> '' then
    if SQL.DestroyTable(Table) then
      Memo.Lines.Add('Таблица ' + Table + ' уничтожена')
    else
      Memo.Lines.Add('Таблица ' + Table + ' не уничтожена');
end;

procedure TForm1.btn_RenameTableClick(Sender: TObject);
var
  TableOld: String;
  TableNew: String;
begin
  TableOld := InputBox('Старое название таблицы',
    'Старое название таблицы', '');
  TableNew := InputBox('Новое название таблицы', 'Новое название таблицы', '');
  if (Trim(TableOld) <> '') and (Trim(TableNew) <> '') then
    if SQL.RenameTable(TableOld, TableNew) then
      Memo.Lines.Add('Таблица  ' + TableOld + ' переименована в ' + TableNew +
        ' успешно')
    else
      Memo.Lines.Add('Таблица  ' + TableOld + ' не переименована')
end;

procedure TForm1.btn_TableExistsClick(Sender: TObject);
var
  Table: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(Table) <> '' then
    if SQL.TableExists(Table) then
      Memo.Lines.Add('Таблица ' + Table + ' есть')
    else
      Memo.Lines.Add('Таблицы ' + Table + ' нет')
end;

{ Column }

procedure TForm1.btn_AddColumnClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Название столбца', 'Название столбца', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.AddColumn(Table, Column) then
      Memo.Lines.Add('Столбец  ' + Column + ' добавлен в ' + Table + ' успешно')
    else
      Memo.Lines.Add('Столбец  ' + Column + ' не добавлен в ' + Table)
end;

procedure TForm1.btn_DestroyColumnClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Название столбца', 'Название столбца', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.DestroyColumn(Table, Column) then
      Memo.Lines.Add('Столбец  ' + Column + ' удален из ' + Table + ' успешно')
    else
      Memo.Lines.Add('Столбец  ' + Column + ' не удален из ' + Table)
end;

procedure TForm1.btn_RenameColumnClick(Sender: TObject);
var
  Table: String;
  ColumnOld: String;
  ColumnNew: String;
  ColumnType: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  ColumnOld := InputBox('Название столбца', 'Название столбца', '');
  ColumnNew := InputBox('Новое название столбца', 'Новое название столбца', '');
  ColumnType := InputBox('Тип данных', 'Тип данных',
    SQL.GetColumnType(Table, ColumnOld));
  if (Trim(Table) <> '') and (Trim(ColumnOld) <> '') and (Trim(ColumnNew) <> '')
    and (Trim(ColumnType) <> '') then
    if SQL.RenameColumn(Table, ColumnOld, ColumnNew, ColumnType) then
      Memo.Lines.Add('Столбец  ' + ColumnOld + ' переименован в ' + ColumnNew +
        ' успешно')
    else
      Memo.Lines.Add('Столбец  ' + ColumnOld + ' не переименован');
end;

procedure TForm1.btn_AddRowClick(Sender: TObject);
var
  Table: String;
  Name, Position: String;
  Salary: Double;
  HireDate: TDate;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Name := InputBox('Name', 'Name', 'John Doe');
  Position := InputBox('Position', 'Position', 'Manager');
  Salary := StrToFloat(InputBox('Salary', 'Salary', '50000,00'));
  HireDate := StrToDate(InputBox('HireDate', 'HireDate', DateToStr(Now)));
  if (Trim(Table) <> '') and (Trim(Name) <> '') and (Trim(Position) <> '') and
    (Trim(FloatToStr(Salary)) <> '') and (Trim(DateToStr(HireDate)) <> '') then
    if SQL.InsertRow(Table, Name, Position, Salary, HireDate) then
      ShowMessage('Новая строка успешно добавлена.')
    else
      ShowMessage('Ошибка при добавлении строки.');
end;

procedure TForm1.btn_ColumnExistsClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Название столбца', 'Название столбца', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    if SQL.ColumnExists(Table, Column) then
      Memo.Lines.Add('Столбец ' + Column + ' есть в ' + Table)
    else
      Memo.Lines.Add('Столбца ' + Column + ' нет в ' + Table)
end;

procedure TForm1.btn_GetColumnTypeClick(Sender: TObject);
var
  Table: String;
  Column: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Название столбца', 'Название столбца', '');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then
    Memo.Lines.Add('Тип данных в столбце ' + Column + ' ' +
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
  Table := InputBox('Название таблицы', 'Название таблицы', '');
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
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  NewName := InputBox('Новое имя', 'Новое Имя', 'Jane Doe');
  if (Trim(Table) <> '') and (Trim(IntToStr(RowID)) <> '') and
    (Trim(NewName) <> '') then
    if SQL.UpdateRowName(Table, RowID, NewName) then
      ShowMessage('Имя успешно обновлено.')
    else
      ShowMessage('Ошибка при обновлении имени.');
end;

procedure TForm1.btn_ChangeLastIdNameClick(Sender: TObject);
var
  Table: String;
  NewName: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  NewName := InputBox('Новое имя', 'Новое Имя', 'Jane Doe');
  if (Trim(Table) <> '') and (Trim(NewName) <> '') then
    if SQL.UpdateLastRowName(Table, NewName) then
      ShowMessage('Имя успешно обновлено.')
    else
      ShowMessage('Ошибка при обновлении имени.');
end;

procedure TForm1.btn_GetValueByIDClick(Sender: TObject);
var
  Table, Column, Value: String;
  RowID: Integer;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Столбец', 'Столбец', 'name');
  RowID := StrToInt(InputBox('ID', 'ID', ''));
  if (Trim(Table) <> '') and (Trim(Column) <> '') and
    (Trim(IntToStr(RowID)) <> '') then
  Value := SQL.GetRowValueByID(Table, RowID, Column);
  Memo.Lines.Add('Имя сотрудника с ID ' + RowID.ToString + ': ' + Value);
end;

procedure TForm1.btn_GetLastIDValueClick(Sender: TObject);
var
  Table, Column, Value: String;
begin
  Table := InputBox('Название таблицы', 'Название таблицы', '');
  Column := InputBox('Столбец', 'Столбец', 'name');
  if (Trim(Table) <> '') and (Trim(Column) <> '') then

  Value := SQL.GetLastRowValue(Table, Column);
  Memo.Lines.Add('Имя последнего сотрудника: ' + Value);
end;

end.
