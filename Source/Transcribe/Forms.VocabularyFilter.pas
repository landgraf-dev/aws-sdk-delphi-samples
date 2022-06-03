unit Forms.VocabularyFilter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.ComCtrls, AWS.Transcribe, AWS.Transcribe.VocabularyFilter;

type
  TVocabularyFilterForm = class(TForm)
    Panel1: TPanel;
    ConfirmButton: TBitBtn;
    CancelButton: TBitBtn;
    PageControl: TPageControl;
    FilterDetailsTab: TTabSheet;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    FilterNameEdit: TEdit;
    LanguageCodeEdit: TComboBox;
    WordsMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
  private
    FVocabularyFilter: TVocabularyFilter;
    function InsertMode: Boolean;
    function GetFilterName: string;
    function GetLanguageCode: string;
    function GetWords: TArray<string>;
  private
    procedure FillLanguageOptions;
    procedure ValidateForm;
    procedure DisplayWords;
  public
    constructor Create(const AVocabularyFilter: TVocabularyFilter); reintroduce; overload;
    procedure SetVocabularyFilter(const AVocabularyFilter: TVocabularyFilter);
    property FilterName: string read GetFilterName;
    property LanguageCode: string read GetLanguageCode;
    property Words: TArray<string> read GetWords;
  end;

implementation

{$R *.dfm}

uses
  Sparkle.Http.Client;

const
  VALID_LANG_CODES = 'af-ZA,ar-AE,ar-SA,cy-GB,da-DK,de-CH,de-DE,en-AB,en-AU,en-GB,en-IE,en-IN,en-US,en-WL,es-ES,es-US,fa-IR,fr-CA,fr-FR,ga-IE,gd-GB,he-IL,hi-IN,id-ID,it-IT,ja-JP,ko-KR,ms-MY,nl-NL,pt-BR,pt-PT,ru-RU,ta-IN,te-IN,tr-TR,zh-CN,zh-TW,th-TH,en-ZA,en-NZ';

{ TFilterForm }

constructor TVocabularyFilterForm.Create(const AVocabularyFilter: TVocabularyFilter);
begin
  inherited Create(nil);
  SetVocabularyFilter(AVocabularyFilter);
end;

procedure TVocabularyFilterForm.SetVocabularyFilter(const AVocabularyFilter: TVocabularyFilter);
begin
  FVocabularyFilter := AVocabularyFilter;
end;

procedure TVocabularyFilterForm.ValidateForm;
begin
  ConfirmButton.Enabled := (FilterName<>'') and (Pos(' ', FilterName)=0) and (WordsMemo.Lines.Text<>'');
end;

function TVocabularyFilterForm.InsertMode: Boolean;
begin
  Result := FVocabularyFilter=nil;
end;

procedure TVocabularyFilterForm.OnEditChange(Sender: TObject);
begin
  ValidateForm;
end;

procedure TVocabularyFilterForm.FillLanguageOptions;
begin
  LanguageCodeEdit.Items.Clear;
  LanguageCodeEdit.Items.CommaText := VALID_LANG_CODES;
end;

procedure TVocabularyFilterForm.FormCreate(Sender: TObject);
begin
  FillLanguageOptions;
end;

procedure TVocabularyFilterForm.FormShow(Sender: TObject);
begin
  if InsertMode then
  begin
    LanguageCodeEdit.ItemIndex := LanguageCodeEdit.Items.IndexOf(TLanguageCode.EnUS.Value); // since sample media is en-US
  end else
  begin
    FilterNameEdit.Text := FVocabularyFilter.VocabularyFilterName;
    LanguageCodeEdit.Style := csDropDown;
    LanguageCodeEdit.Text := FVocabularyFilter.LanguageCode.Value;
    if (FVocabularyFilter.IsSetDownloadUri) then
      DisplayWords;
  end;
  PageControl.ActivePageIndex := 0;
  FilterNameEdit.Enabled := InsertMode;
  LanguageCodeEdit.Enabled := InsertMode;
  ConfirmButton.Visible := InsertMode;
  WordsMemo.Enabled := InsertMode;
end;

procedure TVocabularyFilterForm.DisplayWords;
var
  CLient: THttpClient;
  Response: THttpResponse;
begin
  Client := THttpClient.Create;
  try
    Response := Client.Get(FVocabularyFilter.DownloadUri);
    try
      WordsMemo.Lines.Text := TEncoding.UTF8.GetString(Response.ContentAsBytes);
    finally
      Response.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TVocabularyFilterForm.GetFilterName: string;
begin
  Result := FilterNameEdit.Text;
end;

function TVocabularyFilterForm.GetLanguageCode: string;
begin
  Result := LanguageCodeEdit.Text;
end;

function TVocabularyFilterForm.GetWords: TArray<string>;
begin
  Result := WordsMemo.Lines.ToStringArray;
end;

end.
