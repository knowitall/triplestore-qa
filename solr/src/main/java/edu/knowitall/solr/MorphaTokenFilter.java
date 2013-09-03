package edu.knowitall.solr;

import java.io.IOException;

import org.apache.lucene.analysis.TokenFilter;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import edu.knowitall.tool.stem.MorphaStemmer;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class MorphaTokenFilter extends TokenFilter {

	public MorphaTokenFilter(TokenStream in) {
		super(in);
	}

	private final CharTermAttribute termAtt = addAttribute(CharTermAttribute.class);
	private Map<String, String> stemCache = Collections.synchronizedMap(new LruCache<String, String>(5000));

	private String currentString() {
		return new String(termAtt.buffer(), 0, termAtt.length());
	}

	private String getStem(String str) {
		if (stemCache.containsKey(str))
			return stemCache.get(str);
		else {
			String stem = createStem(str);
			stemCache.put(str, stem);
			return stem;
		}
	}

	private String createStem(String str) {
		String stem;
		synchronized (stemmer) {
			stem = stemmer.stem(str);
		}
		return stem;
	}

	@Override
	public final synchronized boolean incrementToken() throws IOException {
		if (!input.incrementToken())
			return false;
		else {
			String stemmed = getStem(currentString());
			termAtt.setEmpty().append(stemmed);
			return true;
		}
	}

	private static MorphaStemmer stemmer = new MorphaStemmer();
}

@SuppressWarnings("serial")
class LruCache<K, V> extends LinkedHashMap<K, V> {
	
	private final int maxSize;
	
	public LruCache(int maxSize) {
		super(maxSize * 2, 0.75f, true);
		this.maxSize = maxSize;
	}
	
	@Override
	public boolean removeEldestEntry(Map.Entry<K, V> eldest) {
		return (this.size() > maxSize);
	}
}