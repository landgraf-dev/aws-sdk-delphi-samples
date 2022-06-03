program AWSTranscribeSample;

uses
  Vcl.Forms,
  AWS.Transcribe.Transcript in 'AWS.Transcribe.Transcript.pas',
  AWS.Transcribe.VocabularyFilter in 'AWS.Transcribe.VocabularyFilter.pas',
  Forms.TranscriptionJob in 'Forms.TranscriptionJob.pas' {TranscriptionJobForm},
  Forms.VocabularyFilter in 'Forms.VocabularyFilter.pas' {VocabularyFilterForm},
  Forms.Main in 'Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
