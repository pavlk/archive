% demo of Huffman coding / encoding from the SICP book

function mhuff(msg)
clc; set(0, 'RecursionLimit', 5000);

% ---- DEMONSTRATION ------

[bits tree] = encode(msg);
msg2 = decode(bits, tree);

fprintf('\nHuffman coding/decoding demo. \n\n\tMessage:\t"%s"\n\t   Entropy:\t%.6f\n\t   Redundancy:\t%.1f%%\n', msg, entropy(msg), redundancy(msg)); s = num2str(bits);
fprintf('\n\tEncoded:\t"%s"\n\t   Entropy:\t%.6f\n\t   Redundancy:\t%.1f%%\n\n\tDecoded:\t"%s"\n\n',s(s~=' '), entropy(bits), redundancy(bits), msg2);

% ---- IMPLEMENTATION ----

function [H p] = entropy(msg), p = histc(msg, unique(msg)); p = p/sum(p); H = -sum(p.*log2(p));
function R = redundancy(msg), [H p] = entropy(msg); R = ((1-H/log2(numel(p)))*100); 

function tree = build_tree(msg)
u = unique(msg); w = histc(msg,u); [w i] = sort(w); chars = u(i);
for i =1:numel(w) tree(i).left=[]; tree(i).w = w(i); tree(i).chars = chars(i); tree(i).right = []; end
while numel(tree) > 1
    t.left = tree(1); t.right = tree(2); tree(1:2) = []; t.w = t.left.w + t.right.w; t.chars = [t.left.chars t.right.chars];
    i = numel(tree); while (i > 0 & t.w < tree(i).w) i = i-1; end; tree=[tree(1:i) t tree(i+1:end)]; end

function [bits tree] = encode(msg,tree,node)
if ~exist('tree','var') tree = build_tree(msg); end; if ~exist('node','var') node = tree; end
if isempty(msg) | isempty(node.left) bits=[]; return; end
if numel(msg) > 1 bits = [encode(msg(1), tree, node) encode(msg(2:end), tree)]; return; end
if ismember(msg, node.left.chars) bits = [0 encode(msg, tree, node.left)]; else bits=[1 encode(msg, tree, node.right)]; end

function msg = decode(bits, tree, node)
if ~exist('node','var') node = tree; end
if isempty(bits) msg = []; return; end
if bits(1) t = node.right; else t = node.left; end
if isempty(t.left) msg = [t.chars decode(bits(2:end), tree)]; else msg = decode(bits(2:end), tree, t); end



