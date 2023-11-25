import csv
from icalendar import Calendar

def ics_to_csv(ics_file, csv_file):
    # Read the ICS file
    with open(ics_file, 'rb') as ics_file:
        cal = Calendar.from_ical(ics_file.read())

    # Create a CSV file and write header
    with open(csv_file, 'w', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(['Summary', 'Description', 'Start Time', 'End Time'])

        # Iterate through events in the ICS file and write to CSV
        for event in cal.walk('vevent'):
            summary = event.get('summary')
            description = event.get('description', '')
            start_time = event.get('dtstart').dt

            # Check if 'dtend' is present before accessing 'dt' attribute
            end_time = event.get('dtend')
            if end_time:
                end_time = end_time.dt
            else:
                end_time = None

            writer.writerow([summary, description, start_time, end_time])

    print(f"Conversion complete. CSV file saved as {csv_file}")

# Example usage
ics_file_path = 'Feiertage_Deutschland.ics'
csv_file_path = 'Feiertage_Deutschland.csv'

ics_to_csv(ics_file_path, csv_file_path)
