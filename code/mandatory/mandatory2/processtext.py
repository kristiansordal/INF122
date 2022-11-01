import string

def main():
    messages = []
    with open("message_1.json") as file:
        for line in file.readlines():
            line = line.strip()
            if "content" in line and not "\\u" in line:
                line = line.replace('content', '')
                line = line.replace(': ', '')
                line = line.replace('\"', '')
                line = line.replace(',', '')
                messages.append(line)
    file.close()

    with open("mummi.txt", 'w') as mummi:
        for line in messages:
            mummi.write(line + '\n')
    mummi.close()

if __name__ == "__main__":
    main()