from base import ApiUser, DEV_1, OK_RESPONSE
from nose.tools import nottest


class SizecapTest(ApiUser):
    def test_sizecap(self):
        top_block_height = self.top(DEV_1, [])[2]
        response = self.request('block_sizecap', DEV_1, [top_block_height, 10**6])
        if len(response[1]) < 10:
            self.assertEqual(len(response[1]), top_block_height+2)  #2 = 1 for genesis + 1 for magic element (-6)
