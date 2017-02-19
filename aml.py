import _pickle as cPickle

def unpickle(file):
        fo = open(file, 'rb')
        dict = cPickle.load(fo)
        fo.close()
        return dict

print(unpickle("/home/dubey/Downloads/data_batch_1"))
