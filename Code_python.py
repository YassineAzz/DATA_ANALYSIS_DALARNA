import os
import fitz  # PyMuPDF
from googletrans import Translator
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
import pandas as pd
import shutil
from langdetect import detect


########################################################################### translate_code #######################################################################

def translate_text(text, target_lang='en'):
    translator = Translator()
    try:
        translated_text = translator.translate(text, dest=target_lang).text
        return translated_text
    except Exception as e:
        print(f"Translation error: {e}")
        return None

def add_text_to_pdf(pdf_path, text):
    # Check if there is text to add, if not, exit the function
    if text is None:
        print("No text to add.")
        return
    
    # Create a canvas object which will be used to add text to the PDF
    # The canvas is created for the given pdf_path with a page size of 'letter'
    c = canvas.Canvas(pdf_path, pagesize=letter)
    
    # Split the text into lines based on newline characters
    text = text.split('\n')
    
    # Initialize the starting position for the text on the page
    # x, y coordinates (50, 750) place the text towards the top-left of the page
    x, y = 50, 750
    
    # Iterate through each line of text
    for line in text:
        # Draw the current line of text on the canvas at the current x, y position
        c.drawString(x, y, line)
        
        # Move the y position up by 15 pixels for the next line of text
        # This creates a space between lines of text
        y -= 15
        
        # Check if the y position is too close to the bottom of the page
        if y < 50:
            # If so, end the current page and start a new page
            c.showPage()
            # Reset the y position to the top of the new page
            y = 750
    c.save()


def translate_and_save_pdf(original_pdf_path, translated_pdf_path):
    try:
        doc = fitz.open(original_pdf_path)
        total_translated_text = ""
        for page_num, page in enumerate(doc, start=1):
            page_text = page.get_text()
            translated_page_text = translate_text(page_text)
            if translated_page_text:
                total_translated_text += translated_page_text + "\n\n"
            else:
                print(f"Failed to translate page {page_num}.")
        add_text_to_pdf(translated_pdf_path, total_translated_text)
    except Exception as e:
        print(f"Error creating translated PDF: {e}")

def process_pdf_folder(source_folder, destination_folder):
    if not os.path.exists(destination_folder):
        os.makedirs(destination_folder)

    for file in os.listdir(source_folder):
        if file.lower().endswith('.pdf'):
            full_path = os.path.join(source_folder, file)
            translated_pdf_path = os.path.join(destination_folder, f'english_{file}')
            print(f'Processing {file}...')
            translate_and_save_pdf(full_path, translated_pdf_path)
            print(f'{file} translated and saved.')

source_folder = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/pdf_finance/Non English pdfs'
destination_folder = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/pdf_finance/translated_pdfs'

process_pdf_folder(source_folder, destination_folder)


####################################################################### generate_new_xlsx ############################################################################


# Path to your Excel file
excel_path = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/gri_with_txt.xlsx'
# Path to the folder containing your PDFs
pdfs_path = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/'
# Read the Excel file
df = pd.read_excel(excel_path)
# List all PDF files in the specified folder
pdf_files = [f for f in os.listdir(pdfs_path) if f.endswith('.pdf')]
# Filter rows where the file name in column "AQ" exists in the PDF folder
filtered_df = df[df['file'].apply(lambda x: x + '.pdf' in pdf_files)]
# Save the filtered DataFrame to a new Excel file
filtered_df.to_excel('C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/newww.xlsx', index=False)


####################################################################### split_pdfs_by_region ############################################################################

path_excel = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/list_all_pdfs.xlsx'
path_pdfs = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/'
destination_path = 'C:/Users/yassi/OneDrive - Högskolan Dalarna/DATA_FINAL/Region'

df = pd.read_excel(path_excel)

for index, row in df.iterrows():
    region = row['Region']  
    pdf_title = row['file'] + '.pdf'    

    # Build the full PDF path
    full_pdf_path = os.path.join(path_pdfs, pdf_title)

    # Check if the PDF file exists
    if os.path.exists(full_pdf_path):
        # Create a folder for the region if it doesn't already exist
        region_folder = os.path.join(destination_path, region)
        if not os.path.exists(region_folder):
            os.makedirs(region_folder)
        shutil.move(full_pdf_path, os.path.join(region_folder, pdf_title))
    else:
        print(f"Error the file {pdf_title} does not exist in the folder")

####################################################################### split_pdfs_by_language ############################################################################

def trier_pdfs(dossier):
    dossier_anglais = os.path.join(dossier, "English pdfs")
    dossier_autres = os.path.join(dossier, "Non English pdfs")
    os.makedirs(dossier_anglais, exist_ok=True)
    os.makedirs(dossier_autres, exist_ok=True)
    for fichier in os.listdir(dossier):
        if fichier.endswith(".pdf"):
            chemin_complet = os.path.join(dossier, fichier)
            try:
                doc = fitz.open(chemin_complet)
                texte = ""
                for page in doc:
                    texte += page.get_text()
                doc.close()
                langue = detect(texte)
                if langue == "en":
                    shutil.move(chemin_complet, os.path.join(dossier_anglais, fichier))
                else:
                    shutil.move(chemin_complet, os.path.join(dossier_autres, fichier))
            except Exception as e:
                print(f"Error with : {fichier}: {e}")
dossier = "C:/Users/yassi/OneDrive - Högskolan Dalarna/pdf_finance"
trier_pdfs(dossier)
