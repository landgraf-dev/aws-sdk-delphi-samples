program AWSRekognition;

uses
  Vcl.Forms,
  Forms.Main in 'Forms.Main.pas' {Form8},
  Forms.Image in 'Forms.Image.pas' {ImageForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TImageForm, ImageForm);
  Application.Run;
end.
