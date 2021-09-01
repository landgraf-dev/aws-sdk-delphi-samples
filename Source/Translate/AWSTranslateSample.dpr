program AWSTranslateSample;

uses
  Vcl.Forms,
  Forms.Main in 'Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
