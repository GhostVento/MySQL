unit Unit_SQL;

interface

uses
  {System}
  System.SysUtils, System.Classes,
  {Vcl}
  Vcl.Dialogs,
  {FireDAC}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.DApt, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client;

type
  TSQL = class
    FDConnection: TFDConnection;
    FDQuery: TFDQuery;
  private
    FDriver: string;
    FServer: string;
    FPort: string;
    FUserName: string;
    FPassword: string;
    FDatabase: string;
    FCharset: string;
    FLastError: String;
    procedure SetDriver(const Value: string);
    procedure SetServer(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetDatabase(const Value: string);
    procedure SetCharset(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Driver: string read FDriver write SetDriver;
    property Server: string read FServer write SetServer;
    property Port: string read FPort write SetPort;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
    property Database: string read FDatabase write SetDatabase;
    property Charset: string read FCharset write SetCharset;
    property LastError: String read FLastError;
    // Connection
    function Connect: Boolean;
    function Disconnect: Boolean;
    // Database
    function CreateDatabase(DatabaseName, Charset, Collation: String): Boolean;
    function DestroyDatabase(DatabaseName: String): Boolean;
    function GetDatabaseList: TStringList;
    function ChangeDatabase(OldDatabase, NewDatabase, NewCharset, NewCollation: String): Boolean;
    procedure ExecuteSQL(SQL: String);
    function DatabaseExists(DatabaseName: String): Boolean;
    // Table
    function CreateTable(Database, Table: String): Boolean;
    function DestroyTable(Database, Table: String): Boolean;
    function RenameTable(Database, TableOld, TableNew: String): Boolean;
    function TableExists(Database, Table: String): Boolean;
    function GetTablesInfo(Database: String): TStringList;
    // Column
    function AddColumn(Database, Table, Column: String): Boolean;
    function DestroyColumn(Database, Table, Column: String): Boolean;
    function RenameColumn(Database, Table, ColumnOld, ColumnNew, ColumnType: String): Boolean;
    function GetColumnType(Database, Table, Column: String): String;
    function ColumnExists(Database, Table, Column: String): Boolean;
    function GetColumnsInfo(Database, Table: String): TStringList;
    // Rows
    function InsertRow(Database, Table, Name, Position: String; Salary: Double; HireDate: TDate): Boolean;
    function UpdateRowName(Database, Table: String; RowID: Integer; NewName: String): Boolean;
    function UpdateLastRowName(Database, Table: String; NewName: String): Boolean;
    function GetRowValueByID(Database, Table: String; ID: Integer; Column: String): String;
    function GetLastRowValue(Database, Table: String; Column: String): String;
    function FindRowsByName(Database, Table, Name: String): TStringList;
  end;

var
  SQL: TSQL;

implementation

{$HINTS OFF}

{ TSQL }

procedure TSQL.SetDriver(const Value: string);
begin
  FDriver := Value;
  FDConnection.DriverName := FDriver;
end;

procedure TSQL.SetServer(const Value: string);
begin
  FServer := Value;
  FDConnection.Params.Values['Server'] := FServer;
end;

procedure TSQL.SetPort(const Value: string);
begin
  FPort := Value;
  FDConnection.Params.Values['Port'] := FPort;
end;

procedure TSQL.SetUserName(const Value: string);
begin
  FUserName := Value;
  FDConnection.Params.Values['User_Name'] := FUserName;
end;

procedure TSQL.SetPassword(const Value: string);
begin
  FPassword := Value;
  FDConnection.Params.Values['Password'] := FPassword;
end;

procedure TSQL.SetDatabase(const Value: string);
begin
  FDatabase := Value;
  FDConnection.Params.Values['Database'] := FDatabase;
end;

procedure TSQL.SetCharset(const Value: string);
begin
  FCharset := Value;
  FDConnection.Params.Values['CharacterSet'] := FCharset;
end;

constructor TSQL.Create;
begin
  inherited;
  FDConnection := TFDConnection.Create(nil);
  FDConnection.LoginPrompt := False;
  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FDConnection;
end;

destructor TSQL.Destroy;
begin
  FDQuery.Free;
  FDConnection.Free;
  inherited;
end;

{ Database }

function TSQL.CreateDatabase(DatabaseName, Charset, Collation: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text := Format(
      'CREATE DATABASE `%s` CHARACTER SET %s COLLATE %s',
      [DatabaseName, Charset, Collation]
    );
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
end;

function TSQL.DestroyDatabase(DatabaseName: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text := Format('DROP DATABASE `%s`', [DatabaseName]);
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
end;

function TSQL.GetDatabaseList: TStringList;
var
  DatabaseList: TStringList;
begin
  DatabaseList := TStringList.Create;
  try
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT schema_name, default_character_set_name, default_collation_name ' +
        'FROM information_schema.schemata ' +
        'ORDER BY schema_name';
      FDQuery.Open;

      // Перебираем строки результата и добавляем их в список
      while not FDQuery.Eof do
      begin
        DatabaseList.Add(Format('%s: Charset=%s, Collation=%s',
          [FDQuery.FieldByName('schema_name').AsString,
           FDQuery.FieldByName('default_character_set_name').AsString,
           FDQuery.FieldByName('default_collation_name').AsString]));
        FDQuery.Next;
      end;
    except
      on E: Exception do
      begin
        DatabaseList.Add('Error: ' + E.Message);
      end;
    end;
  finally
    Result := DatabaseList; // Возвращаем результат
  end;
end;

function TSQL.ChangeDatabase(OldDatabase, NewDatabase, NewCharset, NewCollation: String): Boolean;
var
  TableName: String;
begin
  Result := False;
  try
    // Создаем новую базу данных с новым именем, Charset и Collation
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text := Format(
      'CREATE DATABASE %s CHARACTER SET %s COLLATE %s',
      [NewDatabase, NewCharset, NewCollation]
    );
    FDQuery.ExecSQL;

    // Получаем список таблиц из старой базы данных
    FDQuery.SQL.Text :=
    'SELECT table_name FROM information_schema.tables WHERE table_schema = :OldDatabase';

    FDQuery.ParamByName('OldDatabase').AsString := OldDatabase;
    FDQuery.Open;

    // Перемещаем каждую таблицу в новую базу данных
    while not FDQuery.Eof do
    begin
      TableName := FDQuery.FieldByName('table_name').AsString;

      // Перенос таблицы
      ExecuteSQL(Format(
        'RENAME TABLE %s.%s TO %s.%s',
        [OldDatabase, TableName, NewDatabase, TableName]
      ));
      FDQuery.Next;
    end;

    // Удаляем старую базу данных
    ExecuteSQL(Format('DROP DATABASE %s', [OldDatabase]));

    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

// Вспомогательная функция для выполнения SQL-запросов без параметров
procedure TSQL.ExecuteSQL(SQL: String);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection;
    Query.SQL.Text := SQL;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TSQL.DatabaseExists(DatabaseName: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выполняем запрос для проверки существования базы данных
    FDQuery.SQL.Text :=
      'SELECT COUNT(*) AS cnt FROM information_schema.schemata WHERE schema_name = :DatabaseName';
    FDQuery.ParamByName('DatabaseName').AsString := DatabaseName;
    FDQuery.Open;

    // Если COUNT > 0, база данных существует
    Result := FDQuery.FieldByName('cnt').AsInteger > 0;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

{ Connection }

function TSQL.Connect: Boolean;
begin
  Result := False;
  if FDConnection.Connected then
  Result := True
  else
  try
    // Подключаемся
    FDConnection.Connected := True;
    Result := FDConnection.Connected;
  except
    on E: Exception do
      FLastError := E.Message;
  end
end;

function TSQL.Disconnect: Boolean;
begin
  Result := False;
  if not FDConnection.Connected then
  Result := True
  else
  try
    FDConnection.Connected := False;
    if not FDConnection.Connected then
      Result := True
    else
      Result := False;
  except
    on E: Exception do
      FLastError := E.Message;
  end
end;

{ Tables }

function TSQL.CreateTable(Database, Table: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Переключаемся на указанную базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Создаём таблицу в указанной базе данных
    FDQuery.SQL.Text := Format(
      'CREATE TABLE `%s` (' +
      'id INT AUTO_INCREMENT PRIMARY KEY, ' +
      'name VARCHAR(100) NOT NULL, ' +
      'position VARCHAR(50), ' +
      'salary DECIMAL(10, 2), ' +
      'hire_date DATE' +
      ');', [Table]
    );
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.DestroyTable(Database, Table: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Переключаемся на указанную базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Удаляем таблицу
    FDQuery.SQL.Text := Format('DROP TABLE IF EXISTS `%s`;', [Table]);
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.RenameTable(Database, TableOld, TableNew: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Переключаемся на указанную базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Переименовываем таблицу
    FDQuery.SQL.Text := Format('RENAME TABLE `%s` TO `%s`;', [TableOld, TableNew]);
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.TableExists(Database, Table: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Проверяем существование таблицы в указанной базе данных
    FDQuery.SQL.Text :=
      'SELECT COUNT(*) FROM information_schema.tables ' +
      'WHERE table_schema = :DatabaseName AND table_name = :TableName';
    FDQuery.ParamByName('DatabaseName').AsString := Database;
    FDQuery.ParamByName('TableName').AsString := Table;
    FDQuery.Open;

    Result := FDQuery.Fields[0].AsInteger > 0;
    // Таблица существует, если COUNT(*) > 0
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.GetTablesInfo(Database: String): TStringList;
var
  TablesList: TStringList;
begin
  TablesList := TStringList.Create;
  try
    try
      FDQuery.Connection := FDConnection;

      // Получаем список таблиц из указанной базы данных
      FDQuery.SQL.Text :=
        'SELECT table_name ' +
        'FROM information_schema.tables ' +
        'WHERE table_schema = :DatabaseName ' +
        'ORDER BY table_name';
      FDQuery.ParamByName('DatabaseName').AsString := Database;
      FDQuery.Open;

      // Перебираем строки результата и добавляем их в список
      while not FDQuery.Eof do
      begin
        TablesList.Add(FDQuery.FieldByName('table_name').AsString);
        FDQuery.Next;
      end;
    except
      on E: Exception do
      begin
        TablesList.Add('Error: ' + E.Message);
      end;
    end;
  finally
    Result := TablesList;
  end;
end;


{ Columns }

function TSQL.AddColumn(Database, Table, Column: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Выполняем запрос добавления столбца
    FDQuery.SQL.Text := Format('ALTER TABLE `%s` ADD COLUMN `%s` VARCHAR(100) DEFAULT NULL;', [Table, Column]);
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.DestroyColumn(Database, Table, Column: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Выполняем запрос удаления столбца
    FDQuery.SQL.Text := Format('ALTER TABLE `%s` DROP COLUMN `%s`', [Table, Column]);
    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.RenameColumn(Database, Table, ColumnOld, ColumnNew, ColumnType: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Выполняем запрос изменения имени столбца
    FDQuery.SQL.Text := Format('ALTER TABLE `%s` CHANGE `%s` `%s` %s', [Table, ColumnOld, ColumnNew, ColumnType]);
    FDQuery.ExecSQL;
    Result := True;
    ShowMessage('Column renamed successfully.');
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.ColumnExists(Database, Table, Column: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Настраиваем SQL-запрос для проверки существования столбца
    FDQuery.SQL.Text :=
      'SELECT COUNT(*) ' +
      'FROM information_schema.columns ' +
      'WHERE table_schema = :DatabaseName AND table_name = :TableName AND column_name = :ColumnName';
    FDQuery.ParamByName('DatabaseName').AsString := Database;
    FDQuery.ParamByName('TableName').AsString := Table;
    FDQuery.ParamByName('ColumnName').AsString := Column;
    FDQuery.Open;

    Result := FDQuery.Fields[0].AsInteger > 0; // Если COUNT(*) > 0, столбец существует
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.GetColumnType(Database, Table, Column: String): String;
begin
  Result := '';
  try
    FDQuery.Connection := FDConnection;

    // Настраиваем SQL-запрос для получения типа столбца
    FDQuery.SQL.Text :=
      'SELECT COLUMN_TYPE ' +
      'FROM information_schema.columns ' +
      'WHERE table_schema = :DatabaseName AND table_name = :TableName AND column_name = :ColumnName';
    FDQuery.ParamByName('DatabaseName').AsString := Database;
    FDQuery.ParamByName('TableName').AsString := Table;
    FDQuery.ParamByName('ColumnName').AsString := Column;
    FDQuery.Open;

    if not FDQuery.IsEmpty then
      Result := FDQuery.Fields[0].AsString // Полный тип данных столбца
    else
      raise Exception.CreateFmt('Column "%s" in Table "%s" not found.', [Column, Table]);
  except
    on E: Exception do
      raise Exception.Create('Error getting data type: ' + E.Message);
  end;
end;

function TSQL.GetColumnsInfo(Database, Table: String): TStringList;
var
  ColumnsList: TStringList;
begin
  ColumnsList := TStringList.Create;
  try
    try
      FDQuery.Connection := FDConnection;

      // Настраиваем SQL-запрос для получения информации о столбцах
      FDQuery.SQL.Text :=
        'SELECT column_name, column_type ' +
        'FROM information_schema.columns ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName ' +
        'ORDER BY ordinal_position';
      FDQuery.ParamByName('DatabaseName').AsString := Database;
      FDQuery.ParamByName('TableName').AsString := Table;
      FDQuery.Open;

      // Перебираем строки результата и добавляем их в список
      while not FDQuery.Eof do
      begin
        ColumnsList.Add(FDQuery.FieldByName('column_name').AsString + ': ' +
                        FDQuery.FieldByName('column_type').AsString);
        FDQuery.Next;
      end;
    except
      on E: Exception do
      begin
        ColumnsList.Add('Error: ' + E.Message);
      end;
    end;
  finally
    Result := ColumnsList; // Возвращаем результат
  end;
end;

{ Rows }

function TSQL.InsertRow(Database, Table, Name, Position: String; Salary: Double; HireDate: TDate): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Выполняем запрос вставки строки
    FDQuery.SQL.Text := Format(
      'INSERT INTO `%s` (name, position, salary, hire_date) ' +
      'VALUES (:Name, :Position, :Salary, :HireDate);',
      [Table]);

    // Привязка параметров
    FDQuery.ParamByName('Name').AsString := Name;
    FDQuery.ParamByName('Position').AsString := Position;
    FDQuery.ParamByName('Salary').AsFloat := Salary;
    FDQuery.ParamByName('HireDate').AsDate := HireDate;

    FDQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.UpdateRowName(Database, Table: String; RowID: Integer; NewName: String): Boolean;
begin
  Result := False;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Выполняем запрос обновления строки
    FDQuery.SQL.Text := Format(
      'UPDATE `%s` SET name = :NewName WHERE id = :RowID;',
      [Table]);

    // Привязка параметров
    FDQuery.ParamByName('NewName').AsString := NewName;
    FDQuery.ParamByName('RowID').AsInteger := RowID;

    FDQuery.ExecSQL;

    Result := True;
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.UpdateLastRowName(Database, Table: String; NewName: String): Boolean;
begin
  Result := False; // По умолчанию считаем, что обновление не удалось
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Обновляем последнюю строку по id, используя промежуточный подзапрос
    FDQuery.SQL.Text := Format(
      'UPDATE `%s` ' +
      'SET name = :NewName ' +
      'WHERE id = ( ' +
      '  SELECT MAX(id) ' +
      '  FROM `%s` ' +
      ');',
      [Table, Table]);

    // Привязываем параметр для нового значения
    FDQuery.ParamByName('NewName').AsString := NewName;

    FDQuery.ExecSQL;

    Result := True; // Обновление успешно
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSQL.GetRowValueByID(Database, Table: String; ID: Integer; Column: String): String;
begin
  Result := ''; // По умолчанию возвращаем пустую строку
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Читаем значение указанного столбца по ID
    FDQuery.SQL.Text := Format(
      'SELECT `%s` FROM `%s` WHERE id = :ID;',
      [Column, Table]);

    FDQuery.ParamByName('ID').AsInteger := ID;

    FDQuery.Open;

    if not FDQuery.IsEmpty then
      Result := FDQuery.FieldByName(Column).AsString
    else
      raise Exception.CreateFmt('Record with ID %d in Table "%s" not found.', [ID, Table]);
  except
    on E: Exception do
    begin
      Result := ''; // Возвращаем пустую строку в случае ошибки
      FLastError := E.Message;
    end;
  end;
end;

function TSQL.GetLastRowValue(Database, Table: String; Column: String): String;
begin
  Result := ''; // По умолчанию возвращаем пустую строку
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Читаем значение указанного столбца последней строки
    FDQuery.SQL.Text := Format(
      'SELECT `%s` FROM `%s` ORDER BY id DESC LIMIT 1;',
      [Column, Table]);

    FDQuery.Open;

    if not FDQuery.IsEmpty then
      Result := FDQuery.FieldByName(Column).AsString
    else
      raise Exception.CreateFmt('There are no records in Table "%s".', [Table]);
  except
    on E: Exception do
    begin
      Result := ''; // Возвращаем пустую строку в случае ошибки
      FLastError := E.Message;
    end;
  end;
end;

function TSQL.FindRowsByName(Database, Table, Name: String): TStringList;
var
  RowData: TStringList;
  i: Integer;
  FieldValues: String;
begin
  RowData := TStringList.Create;
  try
    FDQuery.Connection := FDConnection;

    // Выбираем текущую базу данных
    FDConnection.ExecSQL(Format('USE `%s`', [Database]));

    // Настраиваем SQL-запрос с оператором LIKE
    FDQuery.SQL.Text := Format('SELECT * FROM `%s` WHERE `name` LIKE :Name', [Table]);
    FDQuery.ParamByName('Name').AsString := '%' + Name + '%'; // Подстрока для поиска
    FDQuery.Open;

    // Проверяем, есть ли строки
    if FDQuery.IsEmpty then
    begin
      RowData.Add('No rows found.');
      Exit(RowData);
    end;

    // Перебираем строки результата
    while not FDQuery.Eof do
    begin
      FieldValues := '';
      for i := 0 to FDQuery.FieldCount - 1 do
      begin
        FieldValues := FieldValues + FDQuery.Fields[i].FieldName + ': ' +
          FDQuery.Fields[i].AsString + ' | ';
      end;
      RowData.Add(FieldValues.TrimRight(['|', ' ']));
      FDQuery.Next;
    end;
  except
    on E: Exception do
    begin
      RowData.Add('Error: ' + E.Message);
    end;
  end;

  Result := RowData; // Возвращаем результат
end;



end.
