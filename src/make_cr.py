def get_name(num, size):
    str = ''
    for _ in range(size):
        str += 'a' if num & 1 == 0 else 'd'
        num >>= 1
    return 'c{0}r'.format(str)

def get_body(num, size):
    str = 'a' if num & 1 == 0 else 'd'
    if size == 1:
        return '(c{0}r pair)'.format(str)
    else:
        return '(c{0}r {1})'.format(str, get_body(num >> 1, size - 1))

def generate(n):
    funcs = []
    for size in range(2, n):
        for i in range(0, 1 << size):
            funcs.append(
                '(define ({0} pair) {1})'.format(get_name(i, size), get_body(i, size)))
    return funcs

if __name__ == '__main__':
    for func in generate(5):
        print(func)
