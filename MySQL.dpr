program MySQL;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {Form1},
  Unit_SQL in 'Unit_SQL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
