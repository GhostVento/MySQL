unit Unit_SQL;

interface

uses
  {System}
  System.SysUtils,
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
    function Connect: Boolean;
    function Disconnect: Boolean;
    // Table
    function CreateTable(Table: String): Boolean;
    function DestroyTable(Table: String): Boolean;
    function TableExists(Table: String): Boolean;
    function RenameTable(TableOld, TableNew: String): Boolean;
    // Column
    function AddColumn(Table: String):Boolean;
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
      ShowMessage('Ошибка: ' + E.Message);
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
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end
  else
    Result := False;
end;

function TSQL.CreateTable(Table: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'CREATE TABLE '+ Table +
        '(' +
        '  id INT AUTO_INCREMENT PRIMARY KEY, ' +
        '  name VARCHAR(100) NOT NULL, ' +
        '  position VARCHAR(50), ' +
        '  salary DECIMAL(10, 2), ' +
        '  hire_date DATE ' +
        ');';
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end;
end;

function TSQL.DestroyTable(Table: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := 'DROP TABLE IF EXISTS ' + Table + ';';
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end;
end;

function TSQL.RenameTable(TableOld, TableNew: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := 'RENAME TABLE ' + TableOld + ' TO ' + TableNew + ';';
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end;
end;

{$HINTS OFF}
function TSQL.TableExists(Table: String): Boolean;
begin
  Result := False; // По умолчанию считаем, что таблицы нет
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text :=
        'SELECT COUNT(*) FROM information_schema.tables ' +
        'WHERE table_schema = :DatabaseName AND table_name = :TableName';
      FDQuery.ParamByName('DatabaseName').AsString := FDConnection.Params.Database;
      FDQuery.ParamByName('TableName').AsString := Table;
      FDQuery.Open;

      Result := FDQuery.Fields[0].AsInteger > 0; // Таблица существует, если COUNT(*) > 0
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end;
end;
{$HINTS ON}

function TSQL.AddColumn(Table: String): Boolean;
begin
  try
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;
      FDQuery.SQL.Text := 'ALTER TABLE '+ Table +' ADD COLUMN middle_name VARCHAR(100) DEFAULT NULL;';
      FDQuery.ExecSQL;
      Result := True;
    finally
      FDQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Ошибка: ' + E.Message);
    end;
  end;
end;




end.
