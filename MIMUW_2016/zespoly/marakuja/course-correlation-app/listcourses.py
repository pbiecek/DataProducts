import requests
from lxml import html
import sys
reload(sys)
sys.setdefaultencoding('utf8')


urls = """
https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/szukajPrzedmiotu&jed_org_kod=10000000&method=faculty_groups&cp_showDescriptions=0&cp_showGroupsColumn=0&cp_cdydsDisplayLevel=0&f_tylkoWRejestracji=0&kierujNaPlanyGrupy=0&tab921a_offset=0&tab921a_limit=30&tab921a_order=2a1a&grupaKod=1000-INF1-OBW
https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/szukajPrzedmiotu&jed_org_kod=10000000&method=faculty_groups&cp_showDescriptions=0&cp_showGroupsColumn=0&cp_cdydsDisplayLevel=0&f_tylkoWRejestracji=0&kierujNaPlanyGrupy=0&tab921a_offset=0&tab921a_limit=30&tab921a_order=2a1a&grupaKod=1000-INF2-OBW
https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/szukajPrzedmiotu&jed_org_kod=10000000&method=faculty_groups&cp_showDescriptions=0&cp_showGroupsColumn=0&cp_cdydsDisplayLevel=0&f_tylkoWRejestracji=0&kierujNaPlanyGrupy=0&tab921a_offset=0&tab921a_limit=30&tab921a_order=2a1a&grupaKod=1000-INF3-OBW
https://usosweb.mimuw.edu.pl/kontroler.php?_action=actionx%3Akatalog2%2Fprzedmioty%2FszukajPrzedmiotu%28jed_org_kod%3A10000000%3Bmethod%3Afaculty_groups%3Bcp_showDescriptions%3A0%3Bcp_showGroupsColumn%3A0%3Bcp_cdydsDisplayLevel%3A2%3Bf_tylkoWRejestracji%3A%3BkierujNaPlanyGrupy%3A%3BgrupaKod%3A1000-OBIER%3Btab921a_offset%3A0%3Btab921a_limit%3A30%3Btab921a_order%3A2a1a%29&cp_modified=1
https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/szukajPrzedmiotu&jed_org_kod=10000000&method=faculty_groups&cp_showDescriptions=0&cp_showGroupsColumn=0&cp_cdydsDisplayLevel=0&f_tylkoWRejestracji=0&kierujNaPlanyGrupy=0&grupaKod=1000-OBIER&tab921a_offset=30&tab921a_limit=30&tab921a_order=2a1a
https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2/przedmioty/szukajPrzedmiotu&jed_org_kod=10000000&method=faculty_groups&cp_showDescriptions=0&cp_showGroupsColumn=0&cp_cdydsDisplayLevel=0&f_tylkoWRejestracji=0&kierujNaPlanyGrupy=0&grupaKod=1000-OBIER&tab921a_offset=60&tab921a_limit=30&tab921a_order=2a1a
""".strip().split()


def parse_html(content):
    tree = html.fromstring(content)
    xpath = './/*[@id="layout-c22a"]//tr/td[2]/div[2]/a'
    items = tree.findall(xpath)
    names = [item.text for item in items]
    return names


def fetch_all():
    names = []
    for url in urls:
        response = requests.get(url)
        content = response.content
        names += parse_html(content)
    return names


if __name__ == '__main__':
    print("\n".join(fetch_all()))
