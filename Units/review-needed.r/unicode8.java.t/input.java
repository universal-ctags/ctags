    void ueberweisen ( Konto empfänger, int betrag ) {
	abheben(betrag);
	empfänger.einzahlen(betrag);
    }
