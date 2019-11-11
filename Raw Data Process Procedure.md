# Capstone
Step 1: Use following code to generate a JSON output with txt input, the MB_J.txt part should be following the actual file name

curl -X POST -u "apikey:EaY-7rtEHk_3KJNNHsR38_mo13iUD8N5cgno0HJC0R9D" \
--header "Content-Type: text/plain;charset=utf-8" \
--header "Accept: application/json" \
--data-binary @/Users/Yoga/Desktop/Test/MB_J.txt \
"https://gateway.watsonplatform.net/personality-insights/api/v3/profile?version=2017-10-13"

Step 2: copy and paste the JSON output into a csv converter: http://www.convertcsv.com/json-to-csv.htm

Step 3: export the csv file and name it correspongingly
