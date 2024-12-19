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
  public
    constructor Create;
    destructor Destroy; override;
    // Connection
    function Connect: Boolean;
    function Disconnect: Boolean;
    // Database
    function CreateDatabase(DatabaseName, Charset, Collation: String): Boolean;
    function DestroyDatabase(DatabaseName: String): Boolean;
    function GetDatabaseList: TStringList;
    function ChangeDatabase(OldDatabase, NewDatabase, NewCharset, NewCollation: String): Boolean;
    procedure ExecuteSQL(SQL: String);
    // Table
    function CreateTable(Table: String): Boolean;
    function DestroyTable(Table: String): Boolean;
    function RenameTable(TableOld, TableNew: String): Boolean;
    function TableExists(Table: String): Boolean;
    function GetTablesInfo: TStringList;
    // Column
    function AddColumn(Table, Column: String): Boolean;
    function DestroyColumn(Table, Column: String): Boolean;
    function RenameColumn(Table, ColumnOld, ColumnNew, ColumnType: String): Boolean;
    function GetColumnType(Table, Column: String): String;
    function ColumnExists(Table, Column: String): Boolean;
    function GetColumnsInfo(Table: String): TStringList;
    // Rows
    function InsertRow(Table, Name, Position: String; Salary: Double; HireDate: TDate): Boolean;
    function UpdateRowName(Table: String; RowID: Integer; NewName: String): Boolean;
    function UpdateLastRowName(Table: String; NewName: String): Boolean;
    function GetRowValueByID(Table: String; ID: Integer; Column: String): String;
    function GetLastRowValue(Table: String; Column: String): String;
  end;

const
  Driver = 'MySQL';
  Server = 'localhost';
  Database = 'default';
  UserName = 'root';
  Password = 'root';
  Port = '3306';
  CharacterSet = 'utf8mb4';

var
  SQL: TSQL;

implementation

{$HINTS OFF}

{ TSQL }

constructor TSQL.Create;
begin
  inherited;
  FDConnection := TFDConnection.Create(nil);
  FDConnection.DriverName := Driver;
  FDConnection.Params.Values['Server'] := Server;
  FDConnection.Params.Values['Database'] := Database;
  FDConnection.Params.Values['User_Name'] := UserName;
  FDConnection.Params.Values['Password'] := Password;
  FDConnection.Params.Values['Port'] := Port;
  FDConnection.Params.Values['CharacterSet'] := CharacterSet;
  FDConnection.LoginPrompt := False;
end;

destructor TSQL.Destroy;
begin
  inherited;
  FDConnection.Free;
end;

{ Database }

function TSQL.CreateDatabase(DatabaseName, Charset, Collation: String): Boolean;
begin
  Result := False;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format(
        'CREATE DATABASE `%s` CHARACTER SET %s COLLATE %s',
        [DatabaseName, Charset, Collation]
      );
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error creating database: ' + E.Message);
    end;
  end;
end;

function TSQL.DestroyDatabase(DatabaseName: String): Boolean;
begin
  Result := False;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format('DROP DATABASE `%s`', [DatabaseName]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error deleting database: ' + E.Message);
    end;
  end;
end;

function TSQL.GetDatabaseList: TStringList;
var
  DatabaseList: TStringList;
begin
  DatabaseList := TStringList.Create;
  try
    FDQuery := TFDQuery.Create(nil);
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
    FDQuery.Free;
    Result := DatabaseList; // Возвращаем результат
  end;
end;

function TSQL.ChangeDatabase(OldDatabase, NewDatabase, NewCharset, NewCollation: String): Boolean;
var
  TableName: String;
begin
  Result := False;
  FDQuery := TFDQuery.Create(nil);
  try
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
      begin
        ShowMessage('Error: ' + E.Message);
      end;
    end;
  finally
    FDQuery.Free;
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

{ Connection }

function TSQL.Connect: Boolean;
begin
  if not FDConnection.Connected then
    try
      FDConnection.Connected := True;
      if FDConnection.Connected then
        Result := True
      else
        Result := False;
    except
      on E: Exception do
      begin
        Result := False;
        ShowMessage('Error: ' + E.Message);
      end;
    end
  else
    Result := True;
end;

function TSQL.Disconnect: Boolean;
begin
  if FDConnection.Connected then
    try
      FDConnection.Connected := False;
      if not FDConnection.Connected then
        Result := True
      else
        Result := False;
    except
      on E: Exception do
      begin
        Result := False;
        ShowMessage('Error: ' + E.Message);
      end;
    end
  else
    Result := False;
end;

{ Tables }

function TSQL.CreateTable(Table: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        Format('CREATE TABLE %s (id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(100) NOT NULL, position VARCHAR(50), salary DECIMAL(10, 2), hire_date DATE);',
        [Table]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.DestroyTable(Table: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format('DROP TABLE IF EXISTS %s;',[Table]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.RenameTable(TableOld, TableNew: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format('RENAME TABLE %s TO %s;',[TableOld, TableNew]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.TableExists(Table: String): Boolean;
begin
  Result := False;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := 'SELECT COUNT(*) FROM information_schema.tables ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName';
      FDQuery.ParamByName('DatabaseName').AsString :=
        FDConnection.Params.Database;
      FDQuery.ParamByName('TableName').AsString := Table;
      FDQuery.Open;

      Result := FDQuery.Fields[0].AsInteger > 0;
      // Таблица существует, если COUNT(*) > 0
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.GetTablesInfo: TStringList;
var
  TablesList: TStringList;
begin
  TablesList := TStringList.Create;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT table_name ' +
        'FROM information_schema.tables ' +
        'WHERE table_schema = :DatabaseName ' +
        'ORDER BY table_name';
      FDQuery.ParamByName('DatabaseName').AsString := FDConnection.Params.Database;
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
    FDQuery.Free;
    Result := TablesList;
  end;
end;

{ Columns }

function TSQL.AddColumn(Table, Column: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        Format('ALTER TABLE %s ADD COLUMN %s VARCHAR(100) DEFAULT NULL;',
        [Table, Column]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.DestroyColumn(Table, Column: String): Boolean;
begin
  Result := False;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;

      FDQuery.SQL.Text := Format('ALTER TABLE %s DROP COLUMN %s',
        [Table, Column]);
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error while deleting column: ' + E.Message);
  end;
end;

function TSQL.RenameColumn(Table, ColumnOld, ColumnNew, ColumnType: String): Boolean;
begin
  Result := False;
  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format(
        'ALTER TABLE %s CHANGE %s %s %s',
        [Table, ColumnOld, ColumnNew, ColumnType]
      );
      FDQuery.ExecSQL;
      Result := True;
      ShowMessage('Column renamed successfully.');
    except
      on E: Exception do
      begin
        Result := False;
        ShowMessage('Error: ' + E.Message);
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;

function TSQL.ColumnExists(Table, Column: String): Boolean;
begin
  Result := False;
  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT COUNT(*) ' +
        'FROM information_schema.columns ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName AND column_name = :ColumnName';
      FDQuery.ParamByName('DatabaseName').AsString := FDConnection.Params.Database;
      FDQuery.ParamByName('TableName').AsString := Table;
      FDQuery.ParamByName('ColumnName').AsString := Column;
      FDQuery.Open;

      Result := FDQuery.Fields[0].AsInteger > 0; // Если COUNT(*) > 0, столбец существует
    except
      on E: Exception do
      begin
        Result := False;
        ShowMessage('Error: ' + E.Message);
      end;
    end;
  finally
    FDQuery.Free;
  end;
end;

function TSQL.GetColumnType(Table, Column: String): String;
begin
  Result := '';
  FDQuery := TFDQuery.Create(nil);
  try
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT COLUMN_TYPE ' +
        'FROM information_schema.columns ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName AND column_name = :ColumnName';
      FDQuery.ParamByName('DatabaseName').AsString := FDConnection.Params.Database;
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
  finally
    FDQuery.Free;
  end;
end;

function TSQL.GetColumnsInfo(Table: String): TStringList;
var
  ColumnsList: TStringList;
begin
  ColumnsList := TStringList.Create;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT column_name, column_type ' +
        'FROM information_schema.columns ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName ' +
        'ORDER BY ordinal_position';
      FDQuery.ParamByName('DatabaseName').AsString := FDConnection.Params.Database;
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
    FDQuery.Free;
    Result := ColumnsList; // Возвращаем результат
  end;
end;

{ Rows }

function TSQL.InsertRow(Table, Name, Position: String; Salary: Double; HireDate: TDate): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format(
        'INSERT INTO %s (name, position, salary, hire_date) ' +
        'VALUES (:Name, :Position, :Salary, :HireDate);',
        [Table]);

      // Привязка параметров
      FDQuery.ParamByName('Name').AsString := Name;
      FDQuery.ParamByName('Position').AsString := Position;
      FDQuery.ParamByName('Salary').AsFloat := Salary;
      FDQuery.ParamByName('HireDate').AsDate := HireDate;

      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.UpdateRowName(Table: String; RowID: Integer; NewName: String): Boolean;
begin
  Result := False;
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := Format(
        'UPDATE %s SET name = :NewName WHERE id = :RowID;',
        [Table]
      );

      // Привязка параметров
      FDQuery.ParamByName('NewName').AsString := NewName;
      FDQuery.ParamByName('RowID').AsInteger := RowID;

      FDQuery.ExecSQL;

      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.UpdateLastRowName(Table: String; NewName: String): Boolean;
begin
  Result := False; // По умолчанию считаем, что обновление не удалось
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;

      // Обновляем последнюю строку по id, используя промежуточный подзапрос
      FDQuery.SQL.Text := Format(
        'UPDATE %s ' +
        'SET name = :NewName ' +
        'WHERE id = ( ' +
        '  SELECT max_id ' +
        '  FROM ( ' +
        '    SELECT MAX(id) AS max_id FROM %s ' +
        '  ) AS subquery ' +
        ');',
        [Table, Table]
      );

      // Привязываем параметр для нового значения
      FDQuery.ParamByName('NewName').AsString := NewName;

      FDQuery.ExecSQL;

      Result := True; // Обновление успешно
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.GetRowValueByID(Table: String; ID: Integer; Column: String): String;
begin
  Result := ''; // По умолчанию возвращаем пустую строку
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;

      // Читаем значение указанного столбца по ID
      FDQuery.SQL.Text := Format(
        'SELECT %s FROM %s WHERE id = :ID',
        [Column, Table]
      );

      FDQuery.ParamByName('ID').AsInteger := ID;

      FDQuery.Open;

      if not FDQuery.IsEmpty then
        Result := FDQuery.FieldByName(Column).AsString
      else
        raise Exception.CreateFmt('Record with ID %d in Table "%s" not found.', [ID, Table]);
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := ''; // Возвращаем пустую строку в случае ошибки
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

function TSQL.GetLastRowValue(Table: String; Column: String): String;
begin
  Result := ''; // По умолчанию возвращаем пустую строку
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;

      // Читаем значение указанного столбца последней строки
      FDQuery.SQL.Text := Format(
        'SELECT %s FROM %s ORDER BY id DESC LIMIT 1',
        [Column, Table]
      );

      FDQuery.Open;

      if not FDQuery.IsEmpty then
        Result := FDQuery.FieldByName(Column).AsString
      else
        raise Exception.CreateFmt('There are no records in Table "%s".', [Table]);
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := ''; // Возвращаем пустую строку в случае ошибки
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

end.
