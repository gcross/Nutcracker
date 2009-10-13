#@+leo-ver=4-thin
#@+node:gcross.20091012171148.1392:@thin lrucache.py
#@@language Python

#@+others
#@+node:gcross.20091012171148.1396:Node
class Node(object):
    __slots__ = ["key","next","previous"]
    def __init__(self,key,next):
        self.key = key
        self.previous = None
        self.relink(next)
    def relink(self,next):
        self.next = next
        if next is not None:
            next.previous = self
    def unlink(self):
        if self.previous is not None:
            self.previous.next = self.next
        if self.next is not None:
            self.next.previous = self.previous
#@-node:gcross.20091012171148.1396:Node
#@+node:gcross.20091012171148.1393:LRUCache
class LRUCache(object):
    #@    @+others
    #@+node:gcross.20091012171148.1394:__slots__
    __slots__ = [
        "first_node",
        "last_node",
        "nodes",
        "values",
        "size",
        "maximum_size",
    ]
    #@-node:gcross.20091012171148.1394:__slots__
    #@+node:gcross.20091012171148.1395:__setitem__
    def __setitem__(self,key,value):
        if key in self:
            self.touch(key)
            self.values[key] = value
        else:
            while self.size >= self.maximum_size:
                del self[self.last_node.key]
            node = Node(key,self.first_node)
            self.first_node = node
            if self.last_node is None:
                self.last_node = self.first_node
            self.nodes[key] = node
            self.values[key] = value
            self.size += 1
    #@-node:gcross.20091012171148.1395:__setitem__
    #@+node:gcross.20091012171148.1400:__getitem__
    def __getitem__(self,key):
        value = self.values[key]
        self.touch(key)
        return value
    #@-node:gcross.20091012171148.1400:__getitem__
    #@+node:gcross.20091012171148.1401:__delitem__
    def __delitem__(self,key):
        node = self.nodes[key]
        node.unlink()
        if self.first_node is node:
            self.first_node = node.next
        if self.last_node is node:
            self.last_node = node.previous
        del self.nodes[key]
        del self.values[key]
        self.size -= 1
    #@-node:gcross.20091012171148.1401:__delitem__
    #@+node:gcross.20091012171148.1408:__contains__
    def __contains__(self,key):
        return key in self.values
    #@-node:gcross.20091012171148.1408:__contains__
    #@+node:gcross.20091012171148.1402:__init__
    def __init__(self,maximum_size):
        self.size = 0
        self.maximum_size = maximum_size
        self.nodes = {}
        self.values = {}
        self.first_node = None
        self.last_node = None
    #@-node:gcross.20091012171148.1402:__init__
    #@+node:gcross.20091012171148.1398:touch
    def touch(self,key):
        if self.size == 1:
            return
        node = self.nodes[key]
        if self.first_node is node:
            return
        if self.last_node is node:
            self.last_node = node.previous
        node.unlink()
        node.relink(self.first_node)
        self.first_node = node
    #@-node:gcross.20091012171148.1398:touch
    #@-others
#@-node:gcross.20091012171148.1393:LRUCache
#@+node:gcross.20091012171148.1403:Tests
if __name__ == '__main__':
    import unittest
    from random import randint, random, sample
    from paycheck import *
    #@    @+others
    #@+node:gcross.20091012171148.1409:UnitTests
    class UnitTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091012171148.1410:test_add_1
        def test_add_1(self):
            cache = LRUCache(1)
            cache[1] = 1
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.last_node,cache.nodes[1])
        #@-node:gcross.20091012171148.1410:test_add_1
        #@+node:gcross.20091012171148.1412:test_add_2
        def test_add_2(self):
            cache = LRUCache(2)
            cache[2] = 2
            cache[1] = 1
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.first_node.next,cache.nodes[2])
            self.assertEqual(cache.last_node,cache.nodes[2])
            self.assertEqual(cache.last_node.previous,cache.nodes[1])
        #@-node:gcross.20091012171148.1412:test_add_2
        #@+node:gcross.20091012171148.1414:test_add_3
        def test_add_3(self):
            cache = LRUCache(3)
            cache[3] = 3
            cache[2] = 2
            cache[1] = 1
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.first_node.next,cache.nodes[2])
            self.assertEqual(cache.first_node.next.next,cache.nodes[3])
            self.assertEqual(cache.last_node,cache.nodes[3])
            self.assertEqual(cache.last_node.previous,cache.nodes[2])
            self.assertEqual(cache.last_node.previous.previous,cache.nodes[1])
        #@-node:gcross.20091012171148.1414:test_add_3
        #@+node:gcross.20091012171148.1415:test_add_2_to_capacity_1
        def test_add_2_to_capacity_1(self):
            cache = LRUCache(1)
            cache[2] = 2
            cache[1] = 1
            self.assertTrue(1 in cache)
            self.assertTrue(1 in cache.nodes)
            self.assertTrue(1 in cache.values)
            self.assertTrue(2 not in cache)
            self.assertTrue(2 not in cache.nodes)
            self.assertTrue(2 not in cache.values)
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.last_node,cache.nodes[1])
            self.assertEqual(None,cache.nodes[1].next)
            self.assertEqual(None,cache.nodes[1].previous)
        #@-node:gcross.20091012171148.1415:test_add_2_to_capacity_1
        #@+node:gcross.20091012171148.1417:test_add_3_to_capacity_2
        def test_add_3_to_capacity_2(self):
            cache = LRUCache(2)
            cache[3] = 3
            cache[2] = 2
            cache[1] = 1
            self.assertTrue(1 in cache)
            self.assertTrue(1 in cache.nodes)
            self.assertTrue(1 in cache.values)
            self.assertTrue(2 in cache)
            self.assertTrue(2 in cache.nodes)
            self.assertTrue(2 in cache.values)
            self.assertTrue(3 not in cache)
            self.assertTrue(3 not in cache.nodes)
            self.assertTrue(3 not in cache.values)
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.first_node.next,cache.nodes[2])
            self.assertEqual(cache.last_node,cache.nodes[2])
            self.assertEqual(cache.last_node.previous,cache.nodes[1])
        #@-node:gcross.20091012171148.1417:test_add_3_to_capacity_2
        #@+node:gcross.20091012171148.1419:test_add_3_to_capacity_2_with_touching
        def test_add_3_to_capacity_2_with_touching(self):
            cache = LRUCache(2)
            cache[3] = 3
            cache[2] = 2
            cache[3]
            self.assertEqual(cache.first_node,cache.nodes[3])
            self.assertEqual(cache.first_node.next,cache.nodes[2])
            self.assertEqual(cache.last_node,cache.nodes[2])
            self.assertEqual(cache.last_node.previous,cache.nodes[3])
            cache[1] = 1
            self.assertTrue(1 in cache)
            self.assertTrue(1 in cache.nodes)
            self.assertTrue(1 in cache.values)
            self.assertTrue(2 not in cache)
            self.assertTrue(2 not in cache.nodes)
            self.assertTrue(2 not in cache.values)
            self.assertTrue(3 in cache)
            self.assertTrue(3 in cache.nodes)
            self.assertTrue(3 in cache.values)
            self.assertEqual(cache.first_node,cache.nodes[1])
            self.assertEqual(cache.first_node.next,cache.nodes[3])
            self.assertEqual(cache.last_node,cache.nodes[3])
            self.assertEqual(cache.last_node.previous,cache.nodes[1])
        #@-node:gcross.20091012171148.1419:test_add_3_to_capacity_2_with_touching
        #@-others
    #@-node:gcross.20091012171148.1409:UnitTests
    #@+node:gcross.20091012171148.1404:RandomizedTests
    class RandomizedTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091012171148.1405:test_simple_case
        @with_checker
        def test_simple_case(self,data=[(int,int)]):
            cache = LRUCache(len(data))
            for key, value in data:
                cache[key] = value
            self.assertEqual(cache.maximum_size,cache.size)
            self.assertEqual(cache.maximum_size,len(data))
            for key, value in data:
                self.assertTrue(key in cache)
                self.assertEqual(cache[key],value)
        #@-node:gcross.20091012171148.1405:test_simple_case
        #@+node:gcross.20091012171148.1407:test_maxed_out_cache
        @with_checker
        def test_maxed_out_cache(self,data=[(int,int)]):
            if len(data) <= 1:
                maximum_size = len(data)
            else:
                maximum_size = randint(1,len(data))
            cache = LRUCache(maximum_size)
            for key, value in data:
                cache[key] = value
            self.assertEqual(cache.maximum_size,cache.size)
            self.assertEqual(cache.maximum_size,maximum_size)
            for key, value in data[-maximum_size:]:
                self.assertTrue(key in cache)
                self.assertEqual(cache[key],value)
            for key, value in data[:-maximum_size]:
                self.assertFalse(key in cache)
                try:
                    value = cache[key]
                    self.fail("Contains key that should have been removed!")
                except KeyError:
                    pass
        #@-node:gcross.20091012171148.1407:test_maxed_out_cache
        #@+node:gcross.20091012171148.1421:test_touching
        @with_checker
        def test_touching(self,data=[(int,int)]):
            maximum_size = randint(1,len(data)) if len(data) > 1 else len(data)
            cache = LRUCache(len(data))
            for key, value in data:
                cache[key] = value
            data_to_touch = []
            keys_to_not_touch = []
            indices_to_touch = set(sample(range(len(data)),maximum_size))
            for i, (key,value) in enumerate(data):
                if i in indices_to_touch:
                    data_to_touch.append((key,value))
                    cache[key]
                else:
                    keys_to_not_touch.append(key)
            for i in xrange(len(data)-maximum_size):
                cache[random()] = random()
            for key, value in data_to_touch:
                self.assertTrue(key in cache)
                self.assertEqual(cache[key],value)
            for key in keys_to_not_touch:
                self.assertFalse(key in cache)
                try:
                    value = cache[key]
                    self.fail("Contains key that should have been removed!")
                except KeyError:
                    pass
        #@-node:gcross.20091012171148.1421:test_touching
        #@-others
    #@-node:gcross.20091012171148.1404:RandomizedTests
    #@-others
    unittest.main()
#@-node:gcross.20091012171148.1403:Tests
#@-others
#@-node:gcross.20091012171148.1392:@thin lrucache.py
#@-leo
