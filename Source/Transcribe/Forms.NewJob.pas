unit Forms.NewJob;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TNewJobForm = class(TForm)
    JobNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MediaUriEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    VocabularyFilterEdit: TComboBox;
    Label5: TLabel;
    ContentRedactionEdit: TComboBox;
    ConfirmButton: TBitBtn;
    CancelButton: TBitBtn;
    Label6: TLabel;
    LanguageCodeEdit: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure JobNameEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetJobName: string;
    function GetLanguageCode: string;
    function GetMediaUri: string;
    function GetVocabularyFilter: string;
    function GetContentRedaction: string;
    procedure ValidateForm;
    procedure FillFilterOprions;
    procedure FillLanguageOptions;
  public
    procedure SetAvailableFilter(const AFilterName: string);
    property JobName: string read GetJobName;
    property LanguageCode: string read GetLanguageCode;
    property MediaUri: string read GetMediaUri;
    property VocabularyFilter: string read GetVocabularyFilter;
    property ContentRedaction: string read GetContentRedaction;
  end;

implementation

{$R *.dfm}

uses
  AWS.Transcribe;

const
  SAMPLE_MEDIA_URI = 'https://aws-ml-blog.s3.amazonaws.com/artifacts/transcribe_audio_processing/medical-diarization.wav';
  VALID_LANG_CODES = 'af-ZA,ar-AE,ar-SA,cy-GB,da-DK,de-CH,de-DE,en-AB,en-AU,en-GB,en-IE,en-IN,en-US,en-WL,es-ES,es-US,fa-IR,fr-CA,fr-FR,ga-IE,gd-GB,he-IL,hi-IN,id-ID,it-IT,ja-JP,ko-KR,ms-MY,nl-NL,pt-BR,pt-PT,ru-RU,ta-IN,te-IN,tr-TR,zh-CN,zh-TW,th-TH,en-ZA,en-NZ';

procedure TNewJobForm.FormCreate(Sender: TObject);
begin
  FillLanguageOptions;
  FillFilterOprions;
end;

procedure TNewJobForm.FormShow(Sender: TObject);
begin
  MediaUriEdit.Text := SAMPLE_MEDIA_URI;
  LanguageCodeEdit.ItemIndex := LanguageCodeEdit.Items.IndexOf(TLanguageCode.EnUS.Value); // since sample media is en-US
  VocabularyFilterEdit.ItemIndex := 0;
  ContentRedactionEdit.ItemIndex := 0;
end;

procedure TNewJobForm.FillLanguageOptions;
begin
  LanguageCodeEdit.Items.Clear;
  LanguageCodeEdit.Items.CommaText := VALID_LANG_CODES;
  LanguageCodeEdit.Items.Insert(0, 'Automatically identify language');
end;

procedure TNewJobForm.FillFilterOprions;
begin
  VocabularyFilterEdit.Items.Clear;
  VocabularyFilterEdit.Items.Add('None');
end;

procedure TNewJobForm.SetAvailableFilter(const AFilterName: string);
begin
  VocabularyFilterEdit.Items.Add(AFilterName);
end;

function TNewJobForm.GetJobName: string;
begin
  Result := Trim(JobNameEdit.Text);
end;

function TNewJobForm.GetLanguageCode: string;
begin
  if LanguageCodeEdit.ItemIndex>0 then
    Result := LanguageCodeEdit.Text
  else
    Result := ''; // identify language
end;

function TNewJobForm.GetMediaUri: string;
begin
  Result := MediaUriEdit.Text;
end;

function TNewJobForm.GetVocabularyFilter: string;
begin
  if VocabularyFilterEdit.ItemIndex>0 then
    Result := VocabularyFilterEdit.Text
  else
    Result := ''; // do not apply a vocabulary filter
end;

function TNewJobForm.GetContentRedaction: string;
begin
  case ContentRedactionEdit.ItemIndex of
    1: Result := TRedactionOutput.Redacted.Value;
    2: Result := TRedactionOutput.Redacted_and_unredacted.Value;
  else
    Result := ''; // do not apply content redaction
  end;
end;

procedure TNewJobForm.JobNameEditChange(Sender: TObject);
begin
  ValidateForm;
end;

procedure TNewJobForm.ValidateForm;
begin
  ConfirmButton.Enabled := (JobName<>'') and (Pos(' ', JobName)=0) and (MediaUri<>'');
end;

end.
