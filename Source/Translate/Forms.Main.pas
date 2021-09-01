unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  AWS.Translate, Vcl.Buttons,
  System.StrUtils;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    cbLanguageLeft: TComboBox;
    MemoLeft: TMemo;
    Panel2: TPanel;
    cbLanguageRight: TComboBox;
    MemoRight: TMemo;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    procedure FillLanguageCombo(Combo: TComboBox; const Selection: string);
    function Translate(const SourceText, SourceLanguage, TargetLanguage: string): string;
  public
  end;

type
  TLanguageInfo = record
    Name: string;
    Code: string;
  end;

const
  Languages: array[0..70] of TLanguageInfo = (
    (Name: 'Afrikaans'; Code: 'af'),
    (Name: 'Albanian'; Code: 'sq'),
    (Name: 'Amharic'; Code: 'am'),
    (Name: 'Arabic'; Code: 'ar'),
    (Name: 'Armenian'; Code: 'hy'),
    (Name: 'Azerbaijani'; Code: 'az'),
    (Name: 'Bengali'; Code: 'bn'),
    (Name: 'Bosnian'; Code: 'bs'),
    (Name: 'Bulgarian'; Code: 'bg'),
    (Name: 'Catalan'; Code: 'ca'),
    (Name: 'Chinese (Simplified)'; Code: 'zh'),
    (Name: 'Chinese (Traditional)'; Code: 'zh-TW'),
    (Name: 'Croatian'; Code: 'hr'),
    (Name: 'Czech'; Code: 'cs'),
    (Name: 'Danish'; Code: 'da'),
    (Name: 'Dari'; Code: 'fa-AF'),
    (Name: 'Dutch'; Code: 'nl'),
    (Name: 'English'; Code: 'en'),
    (Name: 'Estonian'; Code: 'et'),
    (Name: 'Farsi (Persian)'; Code: 'fa'),
    (Name: 'Filipino Tagalog'; Code: 'tl'),
    (Name: 'Finnish'; Code: 'fi'),
    (Name: 'French'; Code: 'fr'),
    (Name: 'French (Canada)'; Code: 'fr-CA'),
    (Name: 'Georgian'; Code: 'ka'),
    (Name: 'German'; Code: 'de'),
    (Name: 'Greek'; Code: 'el'),
    (Name: 'Gujarati'; Code: 'gu'),
    (Name: 'Haitian Creole'; Code: 'ht'),
    (Name: 'Hausa'; Code: 'ha'),
    (Name: 'Hebrew'; Code: 'he'),
    (Name: 'Hindi'; Code: 'hi'),
    (Name: 'Hungarian'; Code: 'hu'),
    (Name: 'Icelandic'; Code: 'is'),
    (Name: 'Indonesian'; Code: 'id'),
    (Name: 'Italian'; Code: 'it'),
    (Name: 'Japanese'; Code: 'ja'),
    (Name: 'Kannada'; Code: 'kn'),
    (Name: 'Kazakh'; Code: 'kk'),
    (Name: 'Korean'; Code: 'ko'),
    (Name: 'Latvian'; Code: 'lv'),
    (Name: 'Lithuanian'; Code: 'lt'),
    (Name: 'Macedonian'; Code: 'mk'),
    (Name: 'Malay'; Code: 'ms'),
    (Name: 'Malayalam'; Code: 'ml'),
    (Name: 'Maltese'; Code: 'mt'),
    (Name: 'Mongolian'; Code: 'mn'),
    (Name: 'Norwegian'; Code: 'no'),
    (Name: 'Pashto'; Code: 'ps'),
    (Name: 'Polish'; Code: 'pl'),
    (Name: 'Portuguese'; Code: 'pt'),
    (Name: 'Romanian'; Code: 'ro'),
    (Name: 'Russian'; Code: 'ru'),
    (Name: 'Serbian'; Code: 'sr'),
    (Name: 'Sinhala'; Code: 'si'),
    (Name: 'Slovak'; Code: 'sk'),
    (Name: 'Slovenian'; Code: 'sl'),
    (Name: 'Somali'; Code: 'so'),
    (Name: 'Spanish'; Code: 'es'),
    (Name: 'Spanish (Mexico)'; Code: 'es-MX'),
    (Name: 'Swahili'; Code: 'sw'),
    (Name: 'Swedish'; Code: 'sv'),
    (Name: 'Tamil'; Code: 'ta'),
    (Name: 'Telugu'; Code: 'te'),
    (Name: 'Thai'; Code: 'th'),
    (Name: 'Turkish'; Code: 'tr'),
    (Name: 'Ukrainian'; Code: 'uk'),
    (Name: 'Urdu'; Code: 'ur'),
    (Name: 'Uzbek'; Code: 'uz'),
    (Name: 'Vietnamese'; Code: 'vi'),
    (Name: 'Welsh'; Code: 'cy)')
  );

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FillLanguageCombo(Combo: TComboBox; const Selection: string);
var
  I: Integer;
begin
  Combo.Clear;
  for I := 0 to High(Languages) do
  begin
    Combo.Items.Add(Languages[I].Name);
    if ContainsText(Languages[I].Name, Selection) then
      Combo.ItemIndex := I;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FillLanguageCombo(cbLanguageLeft, 'English');
  FillLanguageCombo(cbLanguageRight, 'German');
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  MemoRight.Lines.Text := Translate(
    MemoLeft.Lines.Text,
    Languages[cbLanguageLeft.ItemIndex].Code,
    Languages[cbLanguageRight.ItemIndex].Code);
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  MemoLeft.Lines.Text := Translate(
    MemoRight.Lines.Text,
    Languages[cbLanguageRight.ItemIndex].Code,
    Languages[cbLanguageLeft.ItemIndex].Code);
end;

function TMainForm.Translate(const SourceText, SourceLanguage, TargetLanguage: string): string;
var
  Client: IAmazonTranslate;
  Request: ITranslateTextRequest;
  Response: ITranslateTextResponse;
begin
  Client := TAmazonTranslateClient.Create;
  Request := TTranslateTextRequest.Create;
  Request.SourceLanguageCode := SourceLanguage;
  Request.TargetLanguageCode := TargetLanguage;
  Request.Text := SourceText;
  Response := Client.TranslateText(Request);
  Result := Response.TranslatedText;
end;

end.

