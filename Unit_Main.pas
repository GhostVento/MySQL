unit Unit_Main;

interface

uses
  {Winapi}
  Winapi.Windows, Winapi.Messages,
  {System}
  System.SysUtils, System.Variants, System.Classes,
  {Vcl}
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btn_Connect: TButton;
    Label1: TLabel;
    lbl_Status: TLabel;
    btn_CreateTable: TButton;
    btn_AddColumn: TButton;
    btn_DestoyTable: TButton;
    btn_RenameTable: TButton;
    btn_TableExists: TButton;
    Timer: TTimer;
    procedure btn_ConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_CreateTableClick(Sender: TObject);
    procedure btn_AddColumnClick(Sender: TObject);
    procedure btn_DestoyTableClick(Sender: TObject);
    procedure btn_RenameTableClick(Sender: TObject);
    procedure btn_TableExistsClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
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

{Connection}

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

{Table}

procedure TForm1.btn_CreateTableClick(Sender: TObject);
var
  TableName: String;
begin
  TableName := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(TableName) <>'' then
    if SQL.CreateTable(TableName) then
    ShowMessage('Таблица  '+TableName+' создана успешно');
end;

procedure TForm1.btn_DestoyTableClick(Sender: TObject);
var
  TableName: String;
begin
  TableName := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(TableName) <>'' then
  if SQL.DestroyTable(TableName) then
  ShowMessage('Таблица '+TableName+' уничтожена успешно');
end;

procedure TForm1.btn_RenameTableClick(Sender: TObject);
var
  TableNameOld: String;
  TableNameNew: String;
begin
  TableNameOld := InputBox('Старое название таблицы', 'Старое название таблицы', '');
  TableNameNew := InputBox('Новое название таблицы', 'Новое название таблицы', '');
  if (Trim(TableNameOld) <>'') and (Trim(TableNameNew) <>'') then
  if SQL.RenameTable(TableNameOld, TableNameNew) then
  ShowMessage('Таблица  '+TableNameOld+' переименована в '+TableNameNew+' успешно');
end;

procedure TForm1.btn_TableExistsClick(Sender: TObject);
var
  TableName: String;
begin
  TableName := InputBox('Название таблицы', 'Название таблицы', '');
  if Trim(TableName) <>'' then
  if SQL.TableExists(TableName) then
    ShowMessage('Таблица есть')
  else
    ShowMessage('Таблицы нет')
end;

{Column}

procedure TForm1.btn_AddColumnClick(Sender: TObject);
begin
  if SQL.AddColumn(InputBox('Название таблицы', 'Название таблицы', '')) then
  ShowMessage('Колонка добавлена успешно');
end;

end.
